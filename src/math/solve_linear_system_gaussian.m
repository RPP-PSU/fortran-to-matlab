function [solution_vector, determinant_value] = solve_linear_system_gaussian(coefficient_matrix, right_hand_side_vector)
    % SOLVE_LINEAR_SYSTEM_GAUSSIAN - Solve linear system using Gaussian elimination with partial pivoting
    %
    % Solves the linear system Ax = b using Gaussian elimination with partial pivoting.
    % This implementation maintains numerical stability through pivoting and provides
    % the determinant as an additional output.
    %
    % Inputs:
    %   coefficient_matrix - Square coefficient matrix A (n x n)
    %   right_hand_side_vector - Right-hand side vector b (n x 1)
    %
    % Outputs:
    %   solution_vector - Solution vector x (n x 1)
    %   determinant_value - Determinant of the coefficient matrix (scalar)
    %
    % This function replaces the FORTRAN subroutine GAUSS
    % Improvements over FORTRAN version:
    % - Input validation and error handling
    % - Better variable names and documentation
    % - MATLAB vectorization where appropriate
    % - Modern programming practices
    %
    % Algorithm: Gaussian elimination with partial pivoting followed by back substitution
    
    % Input validation
    if ~isnumeric(coefficient_matrix) || ~ismatrix(coefficient_matrix)
        error('Coefficient matrix must be a numeric matrix');
    end
    if ~isnumeric(right_hand_side_vector) || ~isvector(right_hand_side_vector)
        error('Right-hand side must be a numeric vector');
    end
    
    [num_rows, num_cols] = size(coefficient_matrix);
    if num_rows ~= num_cols
        error('Coefficient matrix must be square');
    end
    
    vector_length = length(right_hand_side_vector);
    if num_rows ~= vector_length
        error('Matrix and vector dimensions must be compatible');
    end
    
    % Initialize variables
    system_size = num_rows;
    augmented_matrix = coefficient_matrix;  % Work with copy to preserve original
    modified_rhs_vector = right_hand_side_vector(:);  % Ensure column vector
    determinant_value = 1.0;
    
    % Forward elimination with partial pivoting
    for pivot_column = 1:(system_size - 1)
        % Find the row with maximum absolute value in current column (partial pivoting)
        [max_absolute_value, max_row_index] = max(abs(augmented_matrix(pivot_column:system_size, pivot_column)));
        max_row_index = max_row_index + pivot_column - 1;  % Adjust index for subarray
        
        % Check for singularity
        if abs(max_absolute_value) < eps
            error('Matrix is singular or nearly singular');
        end
        
        % Swap rows if necessary
        if max_row_index ~= pivot_column
            % Swap rows in coefficient matrix
            temp_row = augmented_matrix(pivot_column, :);
            augmented_matrix(pivot_column, :) = augmented_matrix(max_row_index, :);
            augmented_matrix(max_row_index, :) = temp_row;
            
            % Swap corresponding elements in RHS vector
            temp_element = modified_rhs_vector(pivot_column);
            modified_rhs_vector(pivot_column) = modified_rhs_vector(max_row_index);
            modified_rhs_vector(max_row_index) = temp_element;
            
            % Update determinant (row swap changes sign)
            determinant_value = -determinant_value;
        end
        
        % Eliminate elements below pivot
        for elimination_row = (pivot_column + 1):system_size
            if abs(augmented_matrix(elimination_row, pivot_column)) > eps
                % Calculate elimination factor
                elimination_factor = augmented_matrix(elimination_row, pivot_column) / ...
                                    augmented_matrix(pivot_column, pivot_column);
                
                % Update row elements (vectorized operation)
                augmented_matrix(elimination_row, (pivot_column + 1):system_size) = ...
                    augmented_matrix(elimination_row, (pivot_column + 1):system_size) - ...
                    elimination_factor * augmented_matrix(pivot_column, (pivot_column + 1):system_size);
                
                % Update RHS vector element
                modified_rhs_vector(elimination_row) = modified_rhs_vector(elimination_row) - ...
                                                      elimination_factor * modified_rhs_vector(pivot_column);
                
                % Set eliminated element to zero (for numerical cleanliness)
                augmented_matrix(elimination_row, pivot_column) = 0;
            end
        end
        
        % Update determinant
        determinant_value = determinant_value * augmented_matrix(pivot_column, pivot_column);
    end
    
    % Final determinant calculation (last diagonal element)
    determinant_value = determinant_value * augmented_matrix(system_size, system_size);
    
    % Check for singularity before back substitution
    if abs(augmented_matrix(system_size, system_size)) < eps
        error('Matrix is singular or nearly singular');
    end
    
    % Back substitution
    solution_vector = zeros(system_size, 1);
    
    % Last element
    solution_vector(system_size) = modified_rhs_vector(system_size) / ...
                                   augmented_matrix(system_size, system_size);
    
    % Remaining elements (working backwards)
    for solution_index = (system_size - 1):-1:1
        temp_sum = modified_rhs_vector(solution_index);
        
        % Subtract known solution components (vectorized)
        temp_sum = temp_sum - sum(augmented_matrix(solution_index, (solution_index + 1):system_size) .* ...
                                 solution_vector((solution_index + 1):system_size)');
        
        solution_vector(solution_index) = temp_sum / augmented_matrix(solution_index, solution_index);
    end
end