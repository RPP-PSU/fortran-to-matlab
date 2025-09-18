function display_vector(vector_label, vector_data)
    % DISPLAY_VECTOR - Display a vector with formatted output
    %
    % Displays a vector with a descriptive label and formatted numerical output.
    % Handles both row and column vectors appropriately.
    %
    % Inputs:
    %   vector_label - Descriptive label for the vector (string)
    %   vector_data - Vector to display (numeric array)
    %
    % This function replaces the FORTRAN subroutine PRINTVEC
    % Improvements over FORTRAN version:
    % - Automatic detection of row/column vectors
    % - Better formatting options
    % - Input validation
    
    % Input validation
    if ~ischar(vector_label) && ~isstring(vector_label)
        error('Vector label must be a string or character array');
    end
    if ~isnumeric(vector_data)
        error('Vector data must be numeric');
    end
    if ~isvector(vector_data)
        error('Input must be a vector (1D array)');
    end
    
    % Display vector label
    fprintf('\n%s\n', vector_label);
    
    % Convert to column vector for consistent display
    vector_data = vector_data(:);
    vector_length = length(vector_data);
    
    % Display vector elements with formatting
    for element_index = 1:vector_length
        fprintf('%12.6f\n', vector_data(element_index));
    end
    fprintf('\n');
    
    % Alternative compact display for small vectors:
    % if vector_length <= 5
    %     fprintf('[');
    %     for element_index = 1:vector_length
    %         fprintf('%8.4f', vector_data(element_index));
    %         if element_index < vector_length
    %             fprintf(', ');
    %         end
    %     end
    %     fprintf(']\n\n');
    % end
end