function apodized_signal = apply_apodization_window(interferogram_signal, window_type)
% APPLY_APODIZATION_WINDOW Applies windowing function to interferogram data
%
% Inputs:
%   interferogram_signal - Vector of interferogram intensity values
%   window_type - String specifying the apodization window type:
%                'happ-genzel' - Happ-Genzel window (default for FT-IR)
%                'hanning' - Hanning window  
%                'hamming' - Hamming window
%                'rectangular' - No apodization (rectangular window)
%
% Outputs:
%   apodized_signal - Windowed interferogram signal
%
% This function applies apodization windows to interferogram data to reduce
% spectral artifacts such as ringing and sidelobes that occur due to the
% finite measurement time. Different window functions offer different trade-offs
% between resolution and artifact suppression.
%
% Window Functions:
% - Happ-Genzel: Optimized for FT-IR spectroscopy, good compromise
% - Hanning: Smooth tapering, reduces sidelobes significantly  
% - Hamming: Similar to Hanning but with different coefficients
% - Rectangular: No windowing, preserves full resolution
%
% Enhanced from original FORTRAN with:
% - Support for multiple window types
% - Vectorized operations for performance
% - Input validation and error handling
% - Detailed documentation of window properties
%
% Author: Translated from FORTRAN for FT-IR applications
% MATLAB Version: 2024 compatible

    % Validate input parameters
    if ~isnumeric(interferogram_signal) || ~isvector(interferogram_signal)
        error('Interferogram signal must be a numeric vector');
    end
    
    if ~ischar(window_type) && ~isstring(window_type)
        error('Window type must be a string or character array');
    end
    
    % Convert to lowercase for case-insensitive comparison
    window_type = lower(string(window_type));
    
    % Get signal length and create index vector
    signal_length = length(interferogram_signal);
    sample_indices = (0:(signal_length-1))';  % Zero-based indexing for window calculations
    
    % Ensure interferogram is a column vector
    interferogram_signal = interferogram_signal(:);
    
    % Calculate apodization window based on specified type
    switch window_type
        case {'happ-genzel', 'happ', 'genzel'}
            % Happ-Genzel window: w(n) = 0.54 + 0.46*cos(π*n/(N-1))
            % Optimized for FT-IR spectroscopy applications
            window_function = 0.54 + 0.46 * cos(pi * sample_indices / (signal_length - 1));
            window_description = 'Happ-Genzel (FT-IR optimized)';
            
        case {'hanning', 'hann'}
            % Hanning window: w(n) = 0.5*(1 + cos(π*n/(N-1)))
            % Good for general spectroscopic applications
            window_function = 0.5 * (1 + cos(pi * sample_indices / (signal_length - 1)));
            window_description = 'Hanning';
            
        case {'hamming', 'hamm'}
            % Hamming window: w(n) = 0.54 - 0.46*cos(2π*n/(N-1))
            % Alternative to Hanning with slightly different characteristics
            window_function = 0.54 - 0.46 * cos(2 * pi * sample_indices / (signal_length - 1));
            window_description = 'Hamming';
            
        case {'rectangular', 'rect', 'none'}
            % Rectangular window: w(n) = 1 (no apodization)
            % Preserves full resolution but may introduce artifacts
            window_function = ones(signal_length, 1);
            window_description = 'Rectangular (no apodization)';
            
        otherwise
            % Default to Happ-Genzel for unknown window types
            fprintf('Warning: Unknown window type "%s", using Happ-Genzel\n', window_type);
            window_function = 0.54 + 0.46 * cos(pi * sample_indices / (signal_length - 1));
            window_description = 'Happ-Genzel (default)';
    end
    
    % Apply apodization window using vectorized multiplication
    apodized_signal = interferogram_signal .* window_function;
    
    % Calculate and display window characteristics for user information
    signal_attenuation = sum(window_function) / signal_length;
    peak_attenuation = min(window_function);
    
    fprintf('Applied %s apodization window\n', window_description);
    fprintf('  Signal points: %d\n', signal_length);
    fprintf('  Average attenuation: %.3f\n', signal_attenuation);
    fprintf('  Peak attenuation: %.3f\n', peak_attenuation);
    
    % Validate output
    if any(~isfinite(apodized_signal))
        warning('Apodized signal contains invalid values');
        apodized_signal(~isfinite(apodized_signal)) = 0;
    end
    
end