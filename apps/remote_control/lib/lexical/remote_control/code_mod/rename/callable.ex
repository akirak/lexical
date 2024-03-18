defmodule Lexical.RemoteControl.CodeMod.Rename.Callable do
  alias Lexical.Ast
  alias Lexical.Ast.Analysis
  alias Lexical.Document.Position
  alias Lexical.Document.Range

  alias Lexical.RemoteControl.CodeIntelligence.Entity
  alias Lexical.RemoteControl.Search.Subject
  alias Lexical.RemoteControl.Search.Store

  alias Sourceror.Zipper

  @spec resolve(Analysis.t(), Position.t()) ::
          {:ok, {atom(), String.t()}, Range.t()} | {:error, atom()}
  def resolve(%Analysis{} = analysis, %Position{} = position) do
    case Entity.resolve(analysis, position) do
      {:ok, {callable, module, local_name, _}, range} when callable in [:call] ->
        {:ok, {:call, {module, local_name}}, function_name_range(analysis, range)}

      _ ->
        {:error, :not_a_callable}
    end
  end

  def rename(%Range{} = _range, _new_name, {module, local_name}) do
    mfa = Subject.mfa(module, local_name, "")
    Store.prefix(mfa, [])
  end

  defp function_name_range(%Analysis{} = analysis, %Range{} = range) do
    case Ast.zipper_at(analysis.document, range.start) do
      {:ok, zipper} ->
        extract_function_name_range(zipper, range)

      {:error, _} ->
        range
    end
  end

  defp extract_function_name_range(%Zipper{node: {function_name, _, _}}, %Range{} = range)
       when is_atom(function_name) do
    range
  end

  defp extract_function_name_range(%Zipper{} = zipper, %Range{} = range) do
    z = Zipper.next(zipper)
    extract_range_for_qualified_call(z.node, range)
  end

  defp extract_range_for_qualified_call(
         {:., meta, [_aliases, function_name]},
         %Range{} = range
       ) do
    dot_length = 1
    start_character = meta[:column] + dot_length
    function_name_length = function_name |> to_string() |> String.length()
    end_character = start_character + function_name_length

    start_position = %{range.start | character: start_character}
    end_position = %{range.start | character: end_character}
    Range.new(start_position, end_position)
  end
end
