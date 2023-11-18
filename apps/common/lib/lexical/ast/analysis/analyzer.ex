defmodule Lexical.Ast.Analysis.Analyzer do
  @moduledoc false

  alias __MODULE__
  alias Lexical.Document
  alias Lexical.Document.Position
  alias Lexical.Document.Range
  alias Sourceror.Zipper

  @scope_id :_scope_id

  @block_keywords [:do, :else, :rescue, :catch, :after]
  @clauses [:->]

  defmodule Alias do
    @enforce_keys [:module, :as, :line]
    defstruct [:module, :as, :line]

    @type t :: %Alias{}

    def new(module, as, line) when is_list(module) and is_atom(as) and line > 0 do
      %Alias{module: module, as: as, line: line}
    end

    def to_module(%Alias{} = alias) do
      Module.concat(alias.module)
    end
  end

  defmodule Import do
    @enforce_keys [:module, :line]
    defstruct [:module, :only, :except, :line]

    @type t :: %Import{}

    def new(module, line, opts \\ [])

    def new(module_name, line, opts) when is_atom(module_name) do
      module_name
      |> Module.split()
      |> Enum.map(&String.to_atom/1)
      |> new(line, opts)
    end

    def new(module, line, opts) when is_list(module) and line > 0 do
      opts = Keyword.validate!(opts, [:only, :except])
      struct!(Import, [module: module, line: line] ++ opts)
    end

    def to_import_spec(%Import{} = import) do
      opts =
        Enum.reject([only: import.only, except: import.except], fn {_, val} -> is_nil(val) end)

      {Module.concat(import.module), opts}
    end

    # From: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#import/2-selector
    #
    # Importing the same module again will erase the previous imports,
    # except when the except option is used, which is always exclusive
    # on a previously declared import/2. If there is no previous import,
    # then it applies to all functions and macros in the module. For
    # example:
    #
    #     import List, only: [flatten: 1, keyfind: 4]
    #     import List, except: [flatten: 1]
    #
    # After the two import calls above, only List.keyfind/4 will be imported.

    # import Foo, only: [bar: 1, baz: 2]
    # import Foo, except: [bar: 1]
    def combine(
          %Import{module: module, only: prev_only},
          %Import{module: module, except: new_except} = new
        )
        when is_list(prev_only) do
      %Import{new | only: prev_only -- new_except, except: nil}
    end

    # import Foo, only: :functions
    # import Foo, except: [bar: 1]
    def combine(
          %Import{module: module, only: prev_only},
          %Import{module: module, except: new_except} = new
        )
        when is_atom(prev_only) do
      %Import{new | only: prev_only, except: new_except}
    end

    def combine(%Import{}, %Import{} = new), do: new
  end

  defmodule Scope do
    @global_imports [Kernel, Kernel.SpecialForms]

    defstruct [
      :id,
      :range,
      module: [],
      aliases: [],
      parent_aliases: %{},
      imports: [],
      parent_imports: %{}
    ]

    @type t :: %Scope{}

    def new(id, %Range{} = range, module, %Scope{} = parent) do
      %Scope{
        id: id,
        range: range,
        module: module,
        parent_aliases: alias_map(parent),
        parent_imports: import_map(parent)
      }
    end

    def global(%Range{} = range) do
      global_imports = Enum.map(@global_imports, &Import.new(&1, range.start.line))
      %Scope{id: :global, range: range, imports: global_imports}
    end

    @spec alias_map(Scope.t(), Position.t() | :end) :: %{atom() => Alias.t()}
    def alias_map(%Scope{} = scope, position \\ :end) do
      scope.aliases
      |> take_before(end_line(scope, position))
      |> Map.new(&{&1.as, &1})
      |> Enum.into(scope.parent_aliases)
    end

    @spec import_map(Scope.t(), Position.t() | :end) :: %{module() => Import.t()}
    def import_map(%Scope{} = scope, position \\ :end) do
      scope.imports
      |> take_before(end_line(scope, position))
      |> Enum.reduce(%{}, fn %Import{} = new, imports ->
        Map.update(imports, new.module, new, &Import.combine(&1, new))
      end)
      |> Map.merge(scope.parent_imports, fn _key, prev, new ->
        Import.combine(prev, new)
      end)
    end

    def empty?(%Scope{aliases: [], imports: []}), do: true
    def empty?(%Scope{}), do: false

    defp take_before(items, line) do
      # sorting by line ensures that items can be processed in the
      # order that they appeared such that later items can override
      # earlier items
      items
      |> Enum.sort_by(& &1.line)
      |> Enum.take_while(&(&1.line <= line))
    end

    defp end_line(%Scope{range: range}, :end), do: range.end.line
    defp end_line(%Scope{}, %Position{line: line}), do: line
  end

  defmodule State do
    defstruct [:document, scopes: [], visited: %{}]

    def new(%Document{} = document) do
      state = %State{document: document}

      global_scope =
        document
        |> global_range()
        |> Scope.global()

      push_scope(state, global_scope)
    end

    def current_scope(%State{scopes: [scope | _]}), do: scope

    def current_module(%State{} = state) do
      current_scope(state).module
    end

    def push_scope(%State{} = state, %Scope{} = scope) do
      Map.update!(state, :scopes, &[scope | &1])
    end

    def push_scope(%State{} = state, id, %Range{} = range, module) when is_list(module) do
      parent_scope = current_scope(state)
      scope = Scope.new(id, range, module, parent_scope)

      push_scope(state, scope)
    end

    def push_scope_for(%State{} = state, quoted, %Range{} = range, module) do
      module = module || current_module(state)
      id = Analyzer.scope_id(quoted)
      push_scope(state, id, range, module)
    end

    def push_scope_for(%State{} = state, quoted, module) do
      range = get_range(quoted, state.document)
      push_scope_for(state, quoted, range, module)
    end

    def maybe_push_scope_for(%State{} = state, quoted) do
      case get_range(quoted, state.document) do
        %Range{} = range ->
          push_scope_for(state, quoted, range, nil)

        nil ->
          state
      end
    end

    def pop_scope(%State{scopes: [scope | rest]} = state) do
      %State{state | scopes: rest, visited: Map.put(state.visited, scope.id, scope)}
    end

    def push(%State{} = state, key, item) do
      update_current_scope(state, fn %Scope{} = scope ->
        Map.update!(scope, key, &[item | &1])
      end)
    end

    defp update_current_scope(%State{} = state, fun) do
      update_in(state, [Access.key(:scopes), Access.at!(0)], fn %Scope{} = scope ->
        fun.(scope)
      end)
    end

    defp get_range(quoted, %Document{} = document) do
      case Sourceror.get_range(quoted) do
        %{start: start_pos, end: end_pos} ->
          Range.new(
            Position.new(document, start_pos[:line], start_pos[:column]),
            Position.new(document, end_pos[:line], end_pos[:column])
          )

        nil ->
          nil
      end
    end

    defp global_range(%Document{} = document) do
      num_lines = Document.size(document)

      Range.new(
        Position.new(document, 1, 1),
        Position.new(document, num_lines + 1, 1)
      )
    end
  end

  @doc """
  Traverses an AST, returning a list of scopes.
  """
  def traverse(quoted, %Document{} = document) do
    quoted = preprocess(quoted)
    initial_state = State.new(document)

    {_, state} =
      Macro.traverse(
        quoted,
        initial_state,
        fn quoted, state ->
          {quoted, analyze_node(quoted, state)}
        end,
        fn quoted, state ->
          case {scope_id(quoted), State.current_scope(state)} do
            {id, %Scope{id: id}} ->
              {quoted, State.pop_scope(state)}

            _ ->
              {quoted, state}
          end
        end
      )

    unless length(state.scopes) == 1 do
      raise RuntimeError,
            "invariant not met, :scopes should only contain the global scope: #{inspect(state)}"
    end

    state
    # pop the final, global state
    |> State.pop_scope()
    |> Map.fetch!(:visited)
    |> Map.reject(fn {_id, scope} -> Scope.empty?(scope) end)
    |> correct_ranges(quoted, document)
    |> Map.values()
  end

  defp preprocess(quoted) do
    Macro.prewalk(quoted, &with_scope_id/1)
  end

  defp correct_ranges(scopes, quoted, document) do
    {_zipper, scopes} =
      quoted
      |> Zipper.zip()
      |> Zipper.traverse(scopes, fn %Zipper{node: node} = zipper, scopes ->
        id = scope_id(node)

        if scope = scopes[id] do
          {zipper, Map.put(scopes, id, maybe_correct_range(scope, zipper, document))}
        else
          {zipper, scopes}
        end
      end)

    scopes
  end

  # extend range for block pairs to either the beginning of their next
  # sibling or, if they are the last element, the end of their parent
  defp maybe_correct_range(scope, %Zipper{node: {_, _}} = zipper, %Document{} = document) do
    with %Zipper{node: sibling} <- Zipper.right(zipper),
         %{start: sibling_start} <- Sourceror.get_range(sibling) do
      new_end = Position.new(document, sibling_start[:line], sibling_start[:column])
      put_in(scope.range.end, new_end)
    else
      _ ->
        # we go up twice to get to the real parent because ast pairs
        # are always in a list
        %Zipper{node: parent} = zipper |> Zipper.up() |> Zipper.up()
        parent_end = Sourceror.get_range(parent).end
        new_end = Position.new(document, parent_end[:line], parent_end[:column])
        put_in(scope.range.end, new_end)
    end
  end

  defp maybe_correct_range(scope, _zipper, _document) do
    scope
  end

  # add a unique ID to 3-element tuples
  defp with_scope_id({_, _, _} = quoted) do
    Macro.update_meta(quoted, &Keyword.put(&1, @scope_id, make_ref()))
  end

  defp with_scope_id(quoted) do
    quoted
  end

  @doc false
  def scope_id({_, meta, _}) when is_list(meta) do
    Keyword.get(meta, @scope_id)
  end

  def scope_id({left, right}) do
    {scope_id(left), scope_id(right)}
  end

  def scope_id(list) when is_list(list) do
    Enum.map(list, &scope_id/1)
  end

  def scope_id(_) do
    nil
  end

  # defmodule Foo do
  defp analyze_node({:defmodule, meta, [{:__aliases__, _, segments} | _]} = quoted, state) do
    module =
      case State.current_module(state) do
        [] -> segments
        current_module -> reify_alias(current_module, segments)
      end

    current_module_alias = Alias.new(module, :__MODULE__, meta[:line])

    state
    # implicit alias belongs to the current scope
    |> maybe_push_implicit_alias(segments, meta[:line])
    # new __MODULE__ alias belongs to the new scope
    |> State.push_scope_for(quoted, module)
    |> State.push(:aliases, current_module_alias)
  end

  # alias Foo.{Bar, Baz, Buzz.Qux}
  defp analyze_node({:alias, meta, [{{:., _, [aliases, :{}]}, _, aliases_nodes}]}, state) do
    base_segments = expand_alias(aliases, state)

    Enum.reduce(aliases_nodes, state, fn {:__aliases__, _, segments}, state ->
      alias = Alias.new(base_segments ++ segments, List.last(segments), meta[:line])
      State.push(state, :aliases, alias)
    end)
  end

  # alias Foo
  # alias Foo.Bar
  # alias __MODULE__.Foo
  defp analyze_node({:alias, meta, [aliases]}, state) do
    case expand_alias(aliases, state) do
      [_ | _] = segments ->
        alias = Alias.new(segments, List.last(segments), meta[:line])
        State.push(state, :aliases, alias)

      [] ->
        state
    end
  end

  # alias Foo, as: Bar
  defp analyze_node({:alias, meta, [aliases, options]}, state) do
    with {:ok, alias_as} <- fetch_alias_as(options),
         [_ | _] = segments <- expand_alias(aliases, state) do
      alias = Alias.new(segments, alias_as, meta[:line])
      State.push(state, :aliases, alias)
    else
      _ ->
        analyze_node({:alias, meta, [aliases]}, state)
    end
  end

  # import Foo
  defp analyze_node({:import, meta, [aliases]}, state) do
    segments = expand_alias(aliases, state)
    import = Import.new(segments, meta[:line])
    State.push(state, :imports, import)
  end

  # import Foo, only: [fun: 1]
  # import Foo, only: :functions
  # import Foo, only: :functions, except: [fun: 1]
  # import Foo, except: [fun: 1]
  defp analyze_node({:import, meta, [aliases, options]}, state) do
    segments = expand_alias(aliases, state)
    import_opts = parse_import_opts(options)
    import = Import.new(segments, meta[:line], import_opts)
    State.push(state, :imports, import)
  end

  # clauses: ->
  defp analyze_node({clause, _, _} = quoted, state) when clause in @clauses do
    State.maybe_push_scope_for(state, quoted)
  end

  # blocks: do, else, etc.
  defp analyze_node({{:__block__, _, [block]}, _} = quoted, state)
       when block in @block_keywords do
    State.maybe_push_scope_for(state, quoted)
  end

  # catch-all
  defp analyze_node(_quoted, state) do
    state
  end

  # don't create an implicit alias if the module is defined using complex forms:
  # defmodule __MODULE__.Foo do
  # defmodule unquote(...) do
  defp maybe_push_implicit_alias(%State{} = state, [non_atom | _], _line)
       when not is_atom(non_atom) do
    state
  end

  defp maybe_push_implicit_alias(%State{} = state, [first_segment | _], line) do
    segments =
      case State.current_module(state) do
        # the head element of top-level modules can be aliased, so we
        # must expand them
        [] ->
          expand_alias([first_segment], state)

        # if we have a current module, we prefix the first segment with it
        current_module ->
          current_module ++ [first_segment]
      end

    implicit_alias = Alias.new(segments, first_segment, line)
    State.push(state, :aliases, implicit_alias)
  end

  defp expand_alias({:__MODULE__, _, nil}, state) do
    State.current_module(state)
  end

  defp expand_alias({:__aliases__, _, segments}, state) do
    expand_alias(segments, state)
  end

  defp expand_alias([{:__MODULE__, _, nil} | segments], state) do
    State.current_module(state) ++ segments
  end

  defp expand_alias([first | rest], state) do
    alias_map = state |> State.current_scope() |> Scope.alias_map()

    case alias_map do
      %{^first => existing_alias} ->
        existing_alias.module ++ rest

      _ ->
        [first | rest]
    end
  end

  defp expand_alias(quoted, state) do
    reify_alias(State.current_module(state), List.wrap(quoted))
  end

  # Expands aliases given the rules in the special form
  # https://hexdocs.pm/elixir/1.13.4/Kernel.SpecialForms.html#__aliases__/1

  # When the head element is the atom :"Elixir", no expansion happens
  defp reify_alias(_, [:"Elixir" | _] = reified) do
    reified
  end

  # Without a current module, we can't expand a non-atom head element
  defp reify_alias([], [non_atom | rest]) when not is_atom(non_atom) do
    rest
  end

  # With no current module and an atom head, no expansion occurs
  defp reify_alias([], [atom | _] = reified) when is_atom(atom) do
    reified
  end

  # Expand current module
  defp reify_alias(current_module, [{:__MODULE__, _, nil} | rest]) do
    current_module ++ rest
  end

  # With a current module and an atom head, the alias is nested in the
  # current module
  defp reify_alias(current_module, [atom | _rest] = reified) when is_atom(atom) do
    current_module ++ reified
  end

  # In other cases, attempt to expand the unreified head element
  defp reify_alias(current_module, [unreified | rest]) do
    env = %Macro.Env{module: current_module}
    reified = Macro.expand(unreified, env)

    if is_atom(reified) do
      [reified | rest]
    else
      rest
    end
  end

  defp fetch_alias_as(options) do
    alias_as =
      Enum.find_value(options, fn
        {{:__block__, _, [:as]}, {:__aliases__, _, [alias_as]}} -> alias_as
        _ -> nil
      end)

    case alias_as do
      nil -> :error
      _ -> {:ok, alias_as}
    end
  end

  defp parse_import_opts(options) when is_list(options) do
    Enum.flat_map(options, fn
      {{:__block__, _, [key]}, {:__block__, _, [val]}} when is_atom(key) and is_atom(val) ->
        [{key, val}]

      {{:__block__, _, [key]}, {:__block__, _, [fun_arities]}}
      when is_atom(key) and is_list(fun_arities) ->
        fun_arities =
          Enum.flat_map(fun_arities, fn
            {{:__block__, _, [fun]}, {:__block__, _, [arity]}} when is_atom(fun) and arity >= 0 ->
              [{fun, arity}]

            _ ->
              []
          end)

        [{key, fun_arities}]

      _ ->
        []
    end)
  end

  defp parse_import_opts(_), do: []
end
