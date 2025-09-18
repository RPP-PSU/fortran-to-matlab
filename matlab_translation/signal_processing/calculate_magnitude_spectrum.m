function absorption_spectrum = calculate_magnitude_spectrum(real_part, imaginary_part)
% CALCULATE_MAGNITUDE_SPECTRUM Computes magnitude spectrum from complex FFT
%
% Inputs:
%   real_part - Real component of the complex spectrum
%   imaginary_part - Imaginary component of the complex spectrum
%
% Outputs:
%   absorption_spectrum - Magnitude spectrum (absorption intensities)
%
% This function calculates the magnitude spectrum from the real and imaginary
% components of a complex FFT result. In FT-IR spectroscopy, the magnitude
% spectrum represents the absorption intensity at each frequency and is the
% final result displayed to users.
%
% Mathematical Operation:
%   magnitude(f) = sqrt(real(f)^2 + imag(f)^2)
%
% This is equivalent to the absolute value of the complex spectrum:
%   magnitude(f) = |complex_spectrum(f)|
%
% Key improvements over original FORTRAN:
% - Vectorized operations for optimal performance
% - Enhanced numerical stability for small values
% - Input validation and error handling
% - Optional spectrum processing and normalization
% - Comprehensive quality checks and diagnostics
%
% Author: Translated from FORTRAN for FT-IR applications
% MATLAB Version: 2024 compatible

    % Validate input parameters
    if ~isnumeric(real_part) || ~isvector(real_part)
        error('Real part must be a numeric vector');
    end
    
    if ~isnumeric(imaginary_part) || ~isvector(imaginary_part)
        error('Imaginary part must be a numeric vector');
    end
    
    % Check that real and imaginary parts have the same length
    if length(real_part) ~= length(imaginary_part)
        error('Real and imaginary parts must have the same length');
    end
    
    % Convert inputs to column vectors for consistency
    real_part = real_part(:);
    imaginary_part = imaginary_part(:);
    spectrum_length = length(real_part);
    
    % Check for valid numerical data
    if any(~isfinite(real_part)) || any(~isfinite(imaginary_part))
        warning('Input spectrum contains invalid values (NaN or Inf)');
        real_part(~isfinite(real_part)) = 0;
        imaginary_part(~isfinite(imaginary_part)) = 0;
    end
    
    % Display magnitude calculation information
    fprintf('Calculating magnitude spectrum:\n');
    fprintf('  Spectrum length: %d points\n', spectrum_length);
    
    % Method 1: Direct vectorized magnitude calculation (most efficient)
    % Uses MATLAB's optimized abs function for complex numbers
    complex_spectrum = complex(real_part, imaginary_part);
    absorption_spectrum = abs(complex_spectrum);
    
    % Alternative Method 2: Explicit calculation (for educational purposes)
    % This method shows the mathematical operation more clearly
    if false  % Set to true to use explicit method for verification
        absorption_spectrum = sqrt(real_part.^2 + imaginary_part.^2);
    end
    
    % Calculate spectrum statistics for quality assessment
    min_intensity = min(absorption_spectrum);
    max_intensity = max(absorption_spectrum);
    mean_intensity = mean(absorption_spectrum);
    std_intensity = std(absorption_spectrum);
    dynamic_range = max_intensity / (min_intensity + eps);
    
    % Count zero and near-zero values
    zero_count = sum(absorption_spectrum == 0);
    near_zero_count = sum(absorption_spectrum < 1e-12 * max_intensity);
    
    fprintf('  Intensity statistics:\n');
    fprintf('    Minimum: %.6e\n', min_intensity);
    fprintf('    Maximum: %.6e\n', max_intensity);
    fprintf('    Mean: %.6e\n', mean_intensity);
    fprintf('    Std Dev: %.6e\n', std_intensity);
    fprintf('    Dynamic range: %.2e\n', dynamic_range);
    fprintf('    Zero values: %d (%.1f%%)\n', zero_count, ...
            100 * zero_count / spectrum_length);
    fprintf('    Near-zero values: %d (%.1f%%)\n', near_zero_count, ...
            100 * near_zero_count / spectrum_length);
    
    % Quality checks and warnings
    if max_intensity <= 0
        warning('Spectrum has no positive values - check input data');
    end
    
    if zero_count > spectrum_length / 2
        warning('More than 50%% of spectrum values are zero');
    end
    
    if dynamic_range < 10
        warning('Low dynamic range (%.1f) - check signal quality', dynamic_range);
    end
    
    if dynamic_range > 1e12
        warning('Very high dynamic range (%.2e) - may indicate numerical issues', ...
                dynamic_range);
    end
    
    % Validate final result
    if any(~isfinite(absorption_spectrum))
        warning('Magnitude calculation produced invalid values');
        absorption_spectrum(~isfinite(absorption_spectrum)) = 0;
    end
    
    % Ensure all values are non-negative (magnitude should always be >= 0)
    if any(absorption_spectrum < 0)
        warning('Negative magnitude values detected - setting to zero');
        absorption_spectrum(absorption_spectrum < 0) = 0;
    end
    
    % Optional: Apply basic spectrum processing
    % These operations can be enabled based on user requirements
    apply_baseline_correction = false;  % Set to true if needed
    apply_normalization = false;        % Set to true if needed
    
    if apply_baseline_correction
        % Simple linear baseline correction
        baseline_points = round(spectrum_length * 0.1);  % Use 10% of points for baseline
        baseline_start = mean(absorption_spectrum(1:baseline_points));
        baseline_end = mean(absorption_spectrum(end-baseline_points+1:end));
        baseline = linspace(baseline_start, baseline_end, spectrum_length)';
        absorption_spectrum = absorption_spectrum - baseline;
        fprintf('  Applied linear baseline correction\n');
    end
    
    if apply_normalization
        % Normalize spectrum to unit maximum
        if max_intensity > 0
            absorption_spectrum = absorption_spectrum / max_intensity;
            fprintf('  Applied unit normalization\n');
        end
    end
    
end