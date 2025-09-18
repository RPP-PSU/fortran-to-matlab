function frequency_axis = generate_frequency_axis(starting_wavenumber, ...
                                                ending_wavenumber, ...
                                                number_of_points)
% GENERATE_FREQUENCY_AXIS Creates frequency axis for FT-IR spectroscopy
%
% Inputs:
%   starting_wavenumber - Starting frequency value in cm^-1
%   ending_wavenumber - Ending frequency value in cm^-1  
%   number_of_points - Number of frequency points to generate
%
% Outputs:
%   frequency_axis - Vector of frequency values in cm^-1
%
% This function generates a linear frequency axis for FT-IR spectroscopy
% applications, typically spanning the mid-infrared region from 4000 to 400 cm^-1.
% The frequency values are evenly spaced in wavenumber units.
%
% Enhanced from original FORTRAN with:
% - Input validation and error checking
% - Vectorized MATLAB operations for performance
% - Flexible frequency range specification
% - Support for both forward and reverse frequency ordering
%
% Example:
%   freq = generate_frequency_axis(4000, 400, 8192);
%   % Creates 8192 points from 4000 to 400 cm^-1
%
% Author: Translated from FORTRAN for FT-IR applications
% MATLAB Version: 2024 compatible

    % Validate input parameters
    if ~isnumeric(starting_wavenumber) || ~isscalar(starting_wavenumber)
        error('Starting wavenumber must be a numeric scalar');
    end
    
    if ~isnumeric(ending_wavenumber) || ~isscalar(ending_wavenumber)
        error('Ending wavenumber must be a numeric scalar');
    end
    
    if ~isnumeric(number_of_points) || ~isscalar(number_of_points) || ...
       number_of_points <= 1 || mod(number_of_points, 1) ~= 0
        error('Number of points must be an integer greater than 1');
    end
    
    % Check for valid frequency range
    if starting_wavenumber == ending_wavenumber
        error('Starting and ending wavenumbers must be different');
    end
    
    % Calculate frequency step size
    frequency_step = (ending_wavenumber - starting_wavenumber) / (number_of_points - 1);
    
    % Generate frequency axis using MATLAB's efficient linspace function
    % This approach is more robust and accurate than the original FORTRAN loop
    frequency_axis = linspace(starting_wavenumber, ending_wavenumber, number_of_points);
    
    % Ensure output is a column vector for consistency with MATLAB conventions
    frequency_axis = frequency_axis(:);
    
    % Validate the generated frequency axis
    if any(~isfinite(frequency_axis))
        error('Generated frequency axis contains invalid values');
    end
    
    % Display frequency axis information for user feedback
    fprintf('Generated frequency axis:\n');
    fprintf('  Range: %.1f to %.1f cm^-1\n', ...
            frequency_axis(1), frequency_axis(end));
    fprintf('  Step size: %.6f cm^-1\n', abs(frequency_step));
    fprintf('  Number of points: %d\n', length(frequency_axis));
    
end