function display_matrix(matrix_label, matrix_data)
    % DISPLAY_MATRIX - Display a matrix with formatted output
    %
    % Displays a matrix with a descriptive label and formatted numerical output.
    % Uses MATLAB's built-in formatting for better readability.
    %
    % Inputs:
    %   matrix_label - Descriptive label for the matrix (string)
    %   matrix_data - Matrix to display (numeric array)
    %
    % This function replaces the FORTRAN subroutine PRINTMAT
    % Improvements over FORTRAN version:
    % - Automatic formatting based on matrix content
    % - Better error handling
    % - More readable output
    
    % Input validation
    if ~ischar(matrix_label) && ~isstring(matrix_label)
        error('Matrix label must be a string or character array');
    end
    if ~isnumeric(matrix_data)
        error('Matrix data must be numeric');
    end
    
    % Display matrix label
    fprintf('\n%s\n', matrix_label);
    
    % Display matrix with appropriate formatting
    [num_rows, num_cols] = size(matrix_data);
    
    % Use MATLAB's built-in display for better formatting
    if num_rows <= 10 && num_cols <= 10
        % For small matrices, use formatted display
        for row_index = 1:num_rows
            for col_index = 1:num_cols
                fprintf('%12.6f', matrix_data(row_index, col_index));
            end
            fprintf('\n');
        end
    else
        % For larger matrices, use MATLAB's default display
        fprintf('Matrix size: %d x %d\n', num_rows, num_cols);
        disp(matrix_data);
    end
    fprintf('\n');
end