function coefficient_matrix = initialize_coefficient_matrix(matrix_dimension)
    % INITIALIZE_COEFFICIENT_MATRIX - Create test coefficient matrix
    %
    % Creates a test coefficient matrix for demonstration purposes.
    % Diagonal elements are set to (2 + row_index), off-diagonal elements
    % are set to 1/(row_index + column_index).
    %
    % Inputs:
    %   matrix_dimension - Size of the square matrix (integer)
    %
    % Outputs:
    %   coefficient_matrix - Initialized coefficient matrix (matrix_dimension x matrix_dimension)
    %
    % This function replaces the FORTRAN subroutine INITMAT
    % Original FORTRAN logic:
    %   IF (I .EQ. J) THEN A(I,J) = 2.0D0 + I
    %   ELSE A(I,J) = 1.0D0 / (I + J)
    
    % Input validation
    if ~isscalar(matrix_dimension) || matrix_dimension <= 0 || ~isnumeric(matrix_dimension)
        error('Matrix dimension must be a positive scalar integer');
    end
    
    % Preallocate matrix for efficiency (MATLAB best practice)
    coefficient_matrix = zeros(matrix_dimension, matrix_dimension);
    
    % Initialize matrix elements using vectorized operations where possible
    for row_index = 1:matrix_dimension
        for column_index = 1:matrix_dimension
            if row_index == column_index
                % Diagonal elements: 2 + row_index
                coefficient_matrix(row_index, column_index) = 2.0 + row_index;
            else
                % Off-diagonal elements: 1/(row_index + column_index)
                coefficient_matrix(row_index, column_index) = 1.0 / (row_index + column_index);
            end
        end
    end
    
    % Alternative vectorized approach (more MATLAB-like):
    % [row_indices, col_indices] = meshgrid(1:matrix_dimension, 1:matrix_dimension);
    % coefficient_matrix = 1 ./ (row_indices + col_indices');
    % coefficient_matrix(1:matrix_dimension+1:end) = 2 + (1:matrix_dimension);
end