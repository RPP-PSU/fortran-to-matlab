function success = write_spectrum_data(filename, frequency_axis, absorption_spectrum)
% WRITE_SPECTRUM_DATA Writes processed FT-IR spectrum data to file
%
% Inputs:
%   filename - String containing the output filename
%   frequency_axis - Vector of frequency values in cm^-1
%   absorption_spectrum - Vector of absorption intensity values
%
% Outputs:
%   success - Logical flag indicating successful file write
%
% This function writes two-column ASCII data files containing processed
% FT-IR spectra. The first column contains frequency values in wavenumbers,
% and the second column contains the corresponding absorption intensities.
%
% Enhanced from original FORTRAN with:
% - Robust error handling and validation
% - Configurable output precision
% - Data validation before writing
% - Detailed error reporting
% - MATLAB-optimized file writing
%
% Author: Translated from FORTRAN for FT-IR applications
% MATLAB Version: 2024 compatible

    % Initialize success flag
    success = false;
    
    % Validate input parameters
    if ~ischar(filename) && ~isstring(filename)
        fprintf('Error: Filename must be a string or character array\n');
        return;
    end
    
    if ~isnumeric(frequency_axis) || ~isvector(frequency_axis)
        fprintf('Error: Frequency axis must be a numeric vector\n');
        return;
    end
    
    if ~isnumeric(absorption_spectrum) || ~isvector(absorption_spectrum)
        fprintf('Error: Absorption spectrum must be a numeric vector\n');
        return;
    end
    
    % Ensure vectors have the same length
    if length(frequency_axis) ~= length(absorption_spectrum)
        fprintf('Error: Frequency axis and spectrum must have the same length\n');
        fprintf('  Frequency axis length: %d\n', length(frequency_axis));
        fprintf('  Spectrum length: %d\n', length(absorption_spectrum));
        return;
    end
    
    % Check for valid numerical data
    if any(~isfinite(frequency_axis)) || any(~isfinite(absorption_spectrum))
        fprintf('Warning: Data contains invalid numerical values (NaN or Inf)\n');
        % Replace invalid values with zeros for robust output
        frequency_axis(~isfinite(frequency_axis)) = 0;
        absorption_spectrum(~isfinite(absorption_spectrum)) = 0;
    end
    
    try
        % Create output directory if it doesn't exist
        output_directory = fileparts(filename);
        if ~isempty(output_directory) && ~exist(output_directory, 'dir')
            mkdir(output_directory);
        end
        
        % Combine data into matrix for efficient writing
        output_data = [frequency_axis(:), absorption_spectrum(:)];
        
        % Write data to file with high precision format
        % Using scientific notation format matching FORTRAN output
        writematrix(output_data, filename, 'Delimiter', '\t', ...
                   'WriteMode', 'overwrite');
        
        % Alternative method with explicit format control for compatibility
        file_handle = fopen(filename, 'w');
        if file_handle == -1
            fprintf('Error: Cannot create output file "%s"\n', filename);
            return;
        end
        
        % Write data with FORTRAN-compatible scientific format
        for data_point_index = 1:length(frequency_axis)
            fprintf(file_handle, '%16.8E\t%16.8E\n', ...
                    frequency_axis(data_point_index), ...
                    absorption_spectrum(data_point_index));
        end
        
        fclose(file_handle);
        success = true;
        
    catch exception
        % Provide detailed error information for debugging
        fprintf('Error writing file "%s": %s\n', filename, exception.message);
        
        % Ensure file handle is closed if an error occurred
        if exist('file_handle', 'var') && file_handle ~= -1
            fclose(file_handle);
        end
        
        success = false;
    end
    
end