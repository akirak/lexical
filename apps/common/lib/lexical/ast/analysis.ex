defmodule Lexical.Ast.Analysis do
  @moduledoc """
  A data structure representing an analyzed AST.

  See `Lexical.Ast.analyze/1`.
  """

  alias Lexical.Ast
  alias Lexical.Ast.Analysis.Analyzer
  alias Lexical.Document
  alias Lexical.Document.Position
  alias Lexical.Document.Range

  defstruct [:ast, :document, :parse_error, scopes: [], valid?: true]

  @type t :: %__MODULE__{
          ast: Macro.t() | nil,
          document: Document.t(),
          parse_error: Ast.parse_error() | nil,
          scopes: [Analyzer.Scope.t()],
          valid?: boolean()
        }

  @doc false
  def new(parse_result, document)

  def new({:ok, ast}, %Document{} = document) do
    scopes = Analyzer.traverse(ast, document)

    %__MODULE__{
      ast: ast,
      document: document,
      scopes: scopes
    }
  end

  def new(error, document) do
    %__MODULE__{
      document: document,
      parse_error: error,
      valid?: false
    }
  end

  @doc false
  @spec aliases_at(t, Position.t()) :: %{atom() => module()}
  def aliases_at(%__MODULE__{} = analysis, %Position{} = position) do
    case fetch_scope_at(analysis, position) do
      {:ok, scope} ->
        scope
        |> Analyzer.Scope.alias_map(position)
        |> Map.new(fn {as, %Analyzer.Alias{} = alias} ->
          {as, Analyzer.Alias.to_module(alias)}
        end)

      :error ->
        %{}
    end
  end

  @doc false
  @spec imports_at(t, Position.t()) :: [import_spec]
        when import_spec: {module(), [import_opt]},
             import_opt:
               {:only, :functions | :macros | :sigils | [fun_arity]}
               | {:except, [fun_arity]},
             fun_arity: {atom(), non_neg_integer()}
  def imports_at(%__MODULE__{} = analysis, %Position{} = position) do
    case fetch_scope_at(analysis, position) do
      {:ok, scope} ->
        scope
        |> Analyzer.Scope.import_map(position)
        |> Map.new(fn {_, %Analyzer.Import{} = import} ->
          Analyzer.Import.to_import_spec(import)
        end)

      :error ->
        %{}
    end
  end

  defp fetch_scope_at(%__MODULE__{} = analysis, %Position{} = position) do
    case scopes_at(analysis, position) do
      [%Analyzer.Scope{} = scope | _] -> {:ok, scope}
      [] -> :error
    end
  end

  defp scopes_at(%__MODULE__{scopes: scopes}, %Position{} = position) do
    scopes
    |> Enum.filter(fn %Analyzer.Scope{range: range} = scope ->
      scope.id == :global or Range.contains?(range, position)
    end)
    |> Enum.sort_by(
      fn
        %Analyzer.Scope{id: :global} -> 0
        %Analyzer.Scope{range: range} -> {range.start.line, range.start.character}
      end,
      :desc
    )
  end
end
