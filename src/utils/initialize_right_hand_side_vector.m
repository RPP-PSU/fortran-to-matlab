function right_hand_side_vector = initialize_right_hand_side_vector(vector_dimension)
    % INITIALIZE_RIGHT_HAND_SIDE_VECTOR - Create test right-hand side vector
    %
    % Creates a test right-hand side vector for the linear system.
    % Each element is set to (index * 2.0).
    %
    % Inputs:
    %   vector_dimension - Size of the vector (integer)
    %
    % Outputs:
    %   right_hand_side_vector - Initialized vector (vector_dimension x 1)
    %
    % This function replaces the FORTRAN subroutine INITVEC
    % Original FORTRAN logic: B(I) = I * 2.0D0
    
    % Input validation
    if ~isscalar(vector_dimension) || vector_dimension <= 0 || ~isnumeric(vector_dimension)
        error('Vector dimension must be a positive scalar integer');
    end
    
    % Create vector using vectorized operation (MATLAB best practice)
    % This is more efficient than the original FORTRAN loop
    right_hand_side_vector = (1:vector_dimension)' * 2.0;
    
    % Alternative explicit loop approach (closer to original FORTRAN):
    % right_hand_side_vector = zeros(vector_dimension, 1);
    % for element_index = 1:vector_dimension
    %     right_hand_side_vector(element_index) = element_index * 2.0;
    % end
end