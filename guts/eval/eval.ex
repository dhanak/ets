defmodule Eval do
  # Meghivás:
  # mix run --no-mix-exs -e 'Eval.start' -- [tester_module] [input] [output]
  #
  # pelda: mix run --no-mix-exs -e 'Eval.start' -- Elixir.Khf1Eval teszt.txt eredmeny.txt
  #
  # @author "lustalista@gmail.com"

  @doc """
  Referencia megoldás futtatása
  """
  @callback reference_solution(any()) :: any()

  @doc """
  Felhasználói megoldás futtatása
  """
  @callback solution(any()) :: any()

  def start() do
    [tester, input_file_name, output_file_name] = System.argv()
    tester_module = String.to_atom(tester)
    reference_solver = fn test_case ->
      apply(tester_module, :reference_solution, [test_case]) # |> IO.inspect
    end
    solver = fn test_case ->
      apply(tester_module, :solution, [test_case])
    end
    test_cases = load_test_cases(input_file_name)
    {:ok, output_file} = File.open(output_file_name, [:write, :binary])
    run_test_cases(test_cases, reference_solver, solver, output_file)
    File.close(output_file)
    System.halt(0)
  end

  defp load_test_cases(input_file_name) do
    try do
      {content, _} = Code.eval_file(input_file_name)
      content
    rescue
      e ->
        exit("#{input_file_name} hibás: #{inspect(e)}")
    end
  end

  defp run_test_cases(test_cases, reference_solver, solver, output_file) do
    Enum.each(test_cases, fn test_case ->
      reference_solution = reference_solver.(test_case)
      solution = try do
                   solver.(test_case)
                 rescue
                   e ->
                     IO.puts("A megoldás kivételt dobott: #{inspect(e)}, stacktrace: #{inspect(__STACKTRACE__)}")
                     :kivetel_tortent
      end
      IO.puts("Elvárt: #{inspect(reference_solution, limit: 1000)}")
      IO.puts("Kapott: #{inspect(solution, limit: 1000)}")
      correct = if reference_solution == solution do
                  1
                else
                  0
      end
      IO.write(output_file, "#{correct}")
    end)
  end
end
