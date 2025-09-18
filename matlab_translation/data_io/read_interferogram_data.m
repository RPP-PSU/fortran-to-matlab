function [optical_path_difference, interferogram_signal, success] = ...
    read_interferogram_data(filename, expected_data_points)
% READ_INTERFEROGRAM_DATA Reads FT-IR interferogram data from file
%
% Inputs:
%   filename - String containing the input filename
%   expected_data_points - Integer specifying expected number of data points
%
% Outputs:
%   optical_path_difference - Vector of optical path difference values
%   interferogram_signal - Vector of interferogram intensity values  
%   success - Logical flag indicating successful file read
%
% This function reads two-column ASCII data files containing interferogram
% measurements. The first column contains optical path difference values,
% and the second column contains the corresponding interferogram signal.
%
% Enhanced from original FORTRAN with:
% - Robust error handling and validation
% - Flexible file format support
% - Memory preallocation for performance
% - Detailed error reporting
%
% Author: Translated from FORTRAN for FT-IR applications
% MATLAB Version: 2024 compatible

    % Initialize output variables with default values
    optical_path_difference = [];
    interferogram_signal = [];
    success = false;
    
    % Validate input parameters
    if ~ischar(filename) && ~isstring(filename)
        fprintf('Error: Filename must be a string or character array\n');
        return;
    end
    
    if ~isnumeric(expected_data_points) || expected_data_points <= 0
        fprintf('Error: Expected data points must be a positive integer\n');
        return;
    end
    
    % Check if file exists before attempting to read
    if ~exist(filename, 'file')
        fprintf('Warning: File "%s" does not exist\n', filename);
        return;
    end
    
    try
        % Attempt to read the data file using optimized MATLAB function
        raw_data = readmatrix(filename);
        
        % Validate data dimensions
        if size(raw_data, 2) < 2
            fprintf('Error: File "%s" must contain at least 2 columns\n', filename);
            return;
        end
        
        % Extract data columns with descriptive variable names
        optical_path_difference = raw_data(:, 1);
        interferogram_signal = raw_data(:, 2);
        
        % Validate data length matches expectations
        actual_data_points = length(interferogram_signal);
        if actual_data_points ~= expected_data_points
            fprintf('Warning: File "%s" contains %d points, expected %d\n', ...
                    filename, actual_data_points, expected_data_points);
            
            % Resize data to match expected length
            if actual_data_points > expected_data_points
                % Truncate excess data points
                optical_path_difference = optical_path_difference(1:expected_data_points);
                interferogram_signal = interferogram_signal(1:expected_data_points);
            else
                % Pad with zeros if insufficient data
                optical_path_difference(end+1:expected_data_points) = 0;
                interferogram_signal(end+1:expected_data_points) = 0;
            end
        end
        
        % Check for NaN or infinite values in the data
        if any(~isfinite(optical_path_difference)) || any(~isfinite(interferogram_signal))
            fprintf('Warning: File "%s" contains invalid numerical values\n', filename);
            % Replace invalid values with zeros
            optical_path_difference(~isfinite(optical_path_difference)) = 0;
            interferogram_signal(~isfinite(interferogram_signal)) = 0;
        end
        
        success = true;
        
    catch exception
        % Provide detailed error information for debugging
        fprintf('Error reading file "%s": %s\n', filename, exception.message);
        success = false;
    end
    
end