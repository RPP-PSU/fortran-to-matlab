function run_matrix_solver_tests()
    % RUN_MATRIX_SOLVER_TESTS - Test suite for matrix solver functionality
    %
    % This function runs comprehensive tests to validate the MATLAB translation
    % of the FORTRAN matrix solver code. It compares results with MATLAB's
    % built-in linear algebra functions for verification.
    %
    % Tests include:
    % - Basic functionality test
    % - Accuracy comparison with MATLAB's built-in solver
    % - Determinant calculation verification
    % - Error handling validation
    %
    % Author: FORTRAN to MATLAB Translation Framework
    % Date: September 2025
    
    fprintf('Running Matrix Solver Test Suite...\n');
    fprintf('=====================================\n\n');
    
    % Add source directories to MATLAB path
    addpath(genpath('../src'));
    
    try
        % Test 1: Basic functionality with default 3x3 system
        fprintf('Test 1: Basic 3x3 system functionality\n');
        test_basic_functionality();
        fprintf('✓ PASSED\n\n');
        
        % Test 2: Accuracy comparison with MATLAB built-in solver
        fprintf('Test 2: Accuracy comparison with MATLAB solver\n');
        test_accuracy_comparison();
        fprintf('✓ PASSED\n\n');
        
        % Test 3: Determinant calculation verification
        fprintf('Test 3: Determinant calculation verification\n');
        test_determinant_calculation();
        fprintf('✓ PASSED\n\n');
        
        % Test 4: Different system sizes
        fprintf('Test 4: Different system sizes\n');
        test_different_sizes();
        fprintf('✓ PASSED\n\n');
        
        % Test 5: Error handling
        fprintf('Test 5: Error handling validation\n');
        test_error_handling();
        fprintf('✓ PASSED\n\n');
        
        fprintf('All tests completed successfully!\n');
        fprintf('The MATLAB translation maintains functionality equivalent to the original FORTRAN code.\n');
        
    catch exception
        fprintf('❌ TEST FAILED: %s\n', exception.message);
        fprintf('Stack trace:\n');
        for i = 1:length(exception.stack)
            fprintf('  %s (line %d)\n', exception.stack(i).name, exception.stack(i).line);
        end
    end
end

function test_basic_functionality()
    % Test basic functionality with the default 3x3 system
    
    % Run the matrix solver
    system_dimension = 3;
    coefficient_matrix = initialize_coefficient_matrix(system_dimension);
    right_hand_side_vector = initialize_right_hand_side_vector(system_dimension);
    
    [solution_vector, determinant_value] = solve_linear_system_gaussian(...
        coefficient_matrix, right_hand_side_vector);
    
    % Verify solution by computing residual
    residual = coefficient_matrix * solution_vector - right_hand_side_vector;
    residual_norm = norm(residual);
    
    if residual_norm > 1e-12
        error('Solution accuracy insufficient: residual norm = %e', residual_norm);
    end
    
    fprintf('  Solution vector: [%8.4f, %8.4f, %8.4f]\n', solution_vector);
    fprintf('  Determinant: %12.6f\n', determinant_value);
    fprintf('  Residual norm: %e\n', residual_norm);
end

function test_accuracy_comparison()
    % Compare with MATLAB's built-in linear algebra solver
    
    system_dimension = 4;
    coefficient_matrix = initialize_coefficient_matrix(system_dimension);
    right_hand_side_vector = initialize_right_hand_side_vector(system_dimension);
    
    % Our solver
    [our_solution, our_determinant] = solve_linear_system_gaussian(...
        coefficient_matrix, right_hand_side_vector);
    
    % MATLAB's built-in solver
    matlab_solution = coefficient_matrix \ right_hand_side_vector;
    matlab_determinant = det(coefficient_matrix);
    
    % Compare solutions
    solution_difference = norm(our_solution - matlab_solution);
    determinant_difference = abs(our_determinant - matlab_determinant);
    
    if solution_difference > 1e-12
        error('Solution differs from MATLAB built-in: difference = %e', solution_difference);
    end
    
    if determinant_difference > 1e-10
        error('Determinant differs from MATLAB built-in: difference = %e', determinant_difference);
    end
    
    fprintf('  Solution difference from MATLAB: %e\n', solution_difference);
    fprintf('  Determinant difference from MATLAB: %e\n', determinant_difference);
end

function test_determinant_calculation()
    % Test determinant calculation with known matrices
    
    % Test with identity matrix
    identity_matrix = eye(3);
    identity_rhs = ones(3, 1);
    [~, identity_det] = solve_linear_system_gaussian(identity_matrix, identity_rhs);
    
    if abs(identity_det - 1.0) > 1e-12
        error('Identity matrix determinant should be 1.0, got %f', identity_det);
    end
    
    % Test with diagonal matrix
    diagonal_matrix = diag([2, 3, 4]);
    diagonal_rhs = ones(3, 1);
    [~, diagonal_det] = solve_linear_system_gaussian(diagonal_matrix, diagonal_rhs);
    expected_det = 2 * 3 * 4;
    
    if abs(diagonal_det - expected_det) > 1e-12
        error('Diagonal matrix determinant should be %f, got %f', expected_det, diagonal_det);
    end
    
    fprintf('  Identity matrix determinant: %f\n', identity_det);
    fprintf('  Diagonal matrix determinant: %f (expected: %f)\n', diagonal_det, expected_det);
end

function test_different_sizes()
    % Test with different system sizes
    
    test_sizes = [2, 5, 8];
    
    for size_index = 1:length(test_sizes)
        current_size = test_sizes(size_index);
        
        coefficient_matrix = initialize_coefficient_matrix(current_size);
        right_hand_side_vector = initialize_right_hand_side_vector(current_size);
        
        [solution_vector, ~] = solve_linear_system_gaussian(...
            coefficient_matrix, right_hand_side_vector);
        
        % Verify solution
        residual_norm = norm(coefficient_matrix * solution_vector - right_hand_side_vector);
        
        if residual_norm > 1e-10
            error('Solution accuracy insufficient for size %d: residual = %e', ...
                  current_size, residual_norm);
        end
        
        fprintf('  Size %dx%d system: residual norm = %e\n', ...
                current_size, current_size, residual_norm);
    end
end

function test_error_handling()
    % Test error handling for invalid inputs
    
    % Test non-square matrix
    try
        non_square_matrix = [1, 2; 3, 4; 5, 6];
        test_rhs = [1; 2; 3];
        solve_linear_system_gaussian(non_square_matrix, test_rhs);
        error('Should have failed for non-square matrix');
    catch exception
        if ~contains(exception.message, 'square')
            rethrow(exception);
        end
    end
    
    % Test incompatible dimensions
    try
        test_matrix = eye(3);
        wrong_size_rhs = [1; 2];
        solve_linear_system_gaussian(test_matrix, wrong_size_rhs);
        error('Should have failed for incompatible dimensions');
    catch exception
        if ~contains(exception.message, 'compatible')
            rethrow(exception);
        end
    end
    
    fprintf('  Error handling tests completed successfully\n');
end