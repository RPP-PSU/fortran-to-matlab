function matrix_solver_main()
    % MATRIX_SOLVER_MAIN - Main driver program for matrix operations and linear equation solver
    %
    % This function demonstrates a complete MATLAB translation of FORTRAN code for
    % solving linear systems using Gaussian elimination with partial pivoting.
    %
    % Translated from FORTRAN code following translation guidelines:
    % - Descriptive variable names
    % - Modular code organization
    % - Clear documentation
    % - MATLAB 2024 compatibility
    %
    % Author: FORTRAN to MATLAB Translation Framework
    % Date: September 2025
    % MATLAB Version: 2024+
    
    % System dimension (equivalent to FORTRAN PARAMETER N = 3)
    system_dimension = 3;
    
    try
        % Initialize coefficient matrix and right-hand side vector
        coefficient_matrix = initialize_coefficient_matrix(system_dimension);
        right_hand_side_vector = initialize_right_hand_side_vector(system_dimension);
        
        % Display input data for verification
        display_matrix('Coefficient Matrix A:', coefficient_matrix);
        display_vector('Right-hand side vector B:', right_hand_side_vector);
        
        % Solve the linear system Ax = b using Gaussian elimination
        [solution_vector, determinant_value] = solve_linear_system_gaussian(...
            coefficient_matrix, right_hand_side_vector);
        
        % Display results
        display_vector('Solution vector X:', solution_vector);
        fprintf('Determinant = %12.6f\n', determinant_value);
        
        % Optional: Verify solution by computing residual
        residual = coefficient_matrix * solution_vector - right_hand_side_vector;
        fprintf('Solution verification (residual norm): %e\n', norm(residual));
        
    catch exception
        fprintf('Error in matrix solver: %s\n', exception.message);
        rethrow(exception);
    end
end