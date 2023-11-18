defmodule Lexical.Ast.Analysis.ImportsTest do
  alias Lexical.Ast

  import Lexical.Test.CursorSupport
  import Lexical.Test.CodeSigil

  use ExUnit.Case

  def imports_at_cursor(text) do
    {position, document} = pop_cursor(text, as: :document)

    document
    |> Ast.analyze()
    |> Ast.Analysis.imports_at(position)
  end

  describe "implicit imports" do
    test "are present without explicit import directive" do
      imports = imports_at_cursor("|")

      assert imports == %{Kernel => [], Kernel.SpecialForms => []}
    end

    test "are overridden by explicit imports" do
      imports = imports_at_cursor(~q[
        import Kernel, only: :functions
        |
      ])

      assert imports == %{Kernel => [only: :functions], Kernel.SpecialForms => []}
    end
  end

  describe "explicit imports" do
    test "can be declared at the top level" do
      imports = imports_at_cursor(~q[
        import Foo
        |
      ])

      assert %{Foo => []} = imports
    end

    test "can be declared with options" do
      imports = imports_at_cursor(~q<
        import Foo, only: [foo: 1]
        import Bar, except: [bar: 1]
        |
      >)

      assert %{Foo => [only: [foo: 1]], Bar => [except: [bar: 1]]} = imports
    end

    test "resolve aliases in scope" do
      imports = imports_at_cursor(~q[
        alias Foo.Bar.Baz
        import Baz
        |
      ])

      assert %{Foo.Bar.Baz => []} = imports
    end

    test "combine :only atom and :except fun-arities" do
      imports = imports_at_cursor(~q<
        import Foo, only: :functions
        import Foo, except: [foo: 1]
        |
      >)

      assert %{Foo => [only: :functions, except: [foo: 1]]} = imports
    end

    test "combine :only fun-arities and :except fun-arities" do
      imports = imports_at_cursor(~q<
        import Foo, only: [foo: 1, foo: 2]
        import Foo, except: [foo: 1]
        |
      >)

      assert %{Foo => [only: [foo: 2]]} = imports
    end

    test "are available in their block" do
      imports = imports_at_cursor(~q<
        if true do
          import Foo
          |
        end
      >)

      assert %{Foo => []} = imports
    end

    test "are unavailable outside their block" do
      imports = imports_at_cursor(~q<
        if true do
          import Foo
        end
        |
      >)

      refute imports[Foo]
    end

    test "are unavailable in sibling blocks" do
      imports = imports_at_cursor(~q<
        if true do
          import Foo
        else
          nil
          |
        end
      >)

      refute imports[Foo]
    end

    test "are not overridden by reimports in an inner scope" do
      imports = imports_at_cursor(~q<
        import Foo, only: [foo: 1]
        if true do
          import Foo, only: [foo: 2]
        end
        |
      >)

      assert %{Foo => [only: [foo: 1]]} = imports
    end
  end
end
