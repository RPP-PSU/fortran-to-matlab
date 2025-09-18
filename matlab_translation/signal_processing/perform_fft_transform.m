function [real_part, imaginary_part] = perform_fft_transform(input_signal)
% PERFORM_FFT_TRANSFORM Computes Fast Fourier Transform for FT-IR spectroscopy
%
% Inputs:
%   input_signal - Vector of real-valued interferogram data
%
% Outputs:
%   real_part - Real component of the FFT result
%   imaginary_part - Imaginary component of the FFT result
%
% This function performs a Fast Fourier Transform (FFT) on interferogram data
% to convert from the time/optical path difference domain to the frequency domain.
% The FFT is the core mathematical operation in FT-IR spectroscopy that enables
% the transformation from interferograms to absorption spectra.
%
% Key improvements over original FORTRAN implementation:
% - Uses MATLAB's highly optimized FFT algorithms
% - Automatic optimal algorithm selection based on signal length
% - Vectorized operations for superior performance
% - Enhanced numerical precision and stability
% - Support for arbitrary signal lengths (not just powers of 2)
% - Comprehensive input validation and error handling
%
% Technical Notes:
% - MATLAB's FFT automatically chooses the most efficient algorithm
% - For power-of-2 lengths, uses radix-2 Cooley-Tukey algorithm
% - For other lengths, uses mixed-radix or chirp-z transform
% - Output scaling matches standard FT-IR conventions
%
% Author: Translated from FORTRAN for FT-IR applications
% MATLAB Version: 2024 compatible

    % Validate input parameters
    if ~isnumeric(input_signal)
        error('Input signal must be numeric');
    end
    
    if ~isvector(input_signal)
        error('Input signal must be a vector');
    end
    
    % Convert to column vector for consistency
    input_signal = input_signal(:);
    signal_length = length(input_signal);
    
    % Check for minimum signal length
    if signal_length < 2
        error('Input signal must contain at least 2 points');
    end
    
    % Check for valid numerical data
    if any(~isfinite(input_signal))
        warning('Input signal contains invalid values (NaN or Inf)');
        % Replace invalid values with zeros for robust processing
        input_signal(~isfinite(input_signal)) = 0;
    end
    
    % Display FFT processing information
    fprintf('Performing FFT transform:\n');
    fprintf('  Signal length: %d points\n', signal_length);
    
    % Check if signal length is a power of 2 for optimal performance
    is_power_of_2 = (signal_length > 0) && (bitand(signal_length, signal_length-1) == 0);
    if is_power_of_2
        fprintf('  Algorithm: Radix-2 Cooley-Tukey (optimal)\n');
    else
        fprintf('  Algorithm: Mixed-radix (signal length not power of 2)\n');
        % Suggest zero-padding for better performance if significantly non-power-of-2
        next_power_of_2 = 2^nextpow2(signal_length);
        if next_power_of_2 / signal_length > 1.5
            fprintf('  Note: Consider zero-padding to %d points for better performance\n', ...
                    next_power_of_2);
        end
    end
    
    % Perform the FFT using MATLAB's optimized algorithm
    % MATLAB's fft function automatically handles:
    % - Optimal algorithm selection
    % - Memory management
    % - Numerical precision optimization
    % - Parallel processing (if available)
    try
        % Start timing for performance monitoring
        fft_start_time = tic;
        
        % Compute complex FFT result
        complex_fft_result = fft(input_signal);
        
        % Extract real and imaginary components
        real_part = real(complex_fft_result);
        imaginary_part = imag(complex_fft_result);
        
        % Record processing time
        fft_time = toc(fft_start_time);
        
        fprintf('  Processing time: %.4f seconds\n', fft_time);
        fprintf('  Performance: %.1f points/second\n', signal_length / fft_time);
        
    catch exception
        error('FFT computation failed: %s', exception.message);
    end
    
    % Validate FFT results
    if any(~isfinite(real_part)) || any(~isfinite(imaginary_part))
        warning('FFT result contains invalid values');
        real_part(~isfinite(real_part)) = 0;
        imaginary_part(~isfinite(imaginary_part)) = 0;
    end
    
    % Calculate and display transform characteristics
    dc_component = real_part(1);  % DC component (zero frequency)
    max_magnitude = max(abs(complex_fft_result));
    
    fprintf('  DC component: %.6e\n', dc_component);
    fprintf('  Maximum magnitude: %.6e\n', max_magnitude);
    
    % Check for potential issues with the transform
    if max_magnitude < 1e-12
        warning('FFT magnitude is very small, check input signal');
    end
    
    if abs(dc_component) > 10 * max_magnitude
        warning('Large DC component detected, consider baseline correction');
    end
    
end