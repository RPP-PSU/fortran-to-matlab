function [corrected_real, corrected_imaginary] = apply_phase_correction(real_part, ...
                                                                      imaginary_part, ...
                                                                      phase_angle)
% APPLY_PHASE_CORRECTION Applies phase correction to complex FFT spectrum
%
% Inputs:
%   real_part - Real component of the complex spectrum
%   imaginary_part - Imaginary component of the complex spectrum  
%   phase_angle - Phase correction angle in radians
%
% Outputs:
%   corrected_real - Phase-corrected real component
%   corrected_imaginary - Phase-corrected imaginary component
%
% This function applies phase correction to complex FFT spectra to properly
% align the absorption features. Phase errors can arise from various sources
% including detector timing, sampling asymmetry, and optical path differences.
% Proper phase correction is essential for obtaining high-quality absorption
% spectra in FT-IR spectroscopy.
%
% Mathematical Operation:
% The phase correction applies a complex rotation:
%   corrected = original * exp(i * phase_angle)
%   corrected = original * (cos(phase) + i*sin(phase))
%
% This is equivalent to the matrix operation:
%   [real_corrected]     [cos(phase)  -sin(phase)] [real_original]
%   [imag_corrected]  =  [sin(phase)   cos(phase)] [imag_original]
%
% Enhanced from original FORTRAN with:
% - Vectorized complex arithmetic for performance
% - Input validation and error handling
% - Support for multiple phase correction methods
% - Detailed mathematical documentation
% - Numerical stability improvements
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
    
    if ~isnumeric(phase_angle) || ~isscalar(phase_angle)
        error('Phase angle must be a numeric scalar');
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
    
    % Validate phase angle range (warn if outside typical range)
    if abs(phase_angle) > 2*pi
        fprintf('Warning: Phase angle %.3f rad (%.1f°) is large\n', ...
                phase_angle, phase_angle * 180/pi);
        fprintf('  Consider normalizing to [-π, π] range\n');
    end
    
    % Display phase correction information
    fprintf('Applying phase correction:\n');
    fprintf('  Spectrum length: %d points\n', spectrum_length);
    fprintf('  Phase angle: %.6f radians (%.2f degrees)\n', ...
            phase_angle, phase_angle * 180/pi);
    
    % Calculate trigonometric functions once for efficiency
    cosine_phase = cos(phase_angle);
    sine_phase = sin(phase_angle);
    
    fprintf('  cos(phase): %.6f\n', cosine_phase);
    fprintf('  sin(phase): %.6f\n', sine_phase);
    
    % Method 1: Direct vectorized complex arithmetic (most efficient)
    % Create complex spectrum and apply phase rotation
    complex_spectrum = complex(real_part, imaginary_part);
    phase_factor = complex(cosine_phase, sine_phase);
    corrected_spectrum = complex_spectrum * phase_factor;
    
    % Extract corrected real and imaginary parts
    corrected_real = real(corrected_spectrum);
    corrected_imaginary = imag(corrected_spectrum);
    
    % Alternative Method 2: Explicit matrix operations (for educational purposes)
    % This method shows the mathematical operations more clearly but is slower
    if false  % Set to true to use explicit method for verification
        % Apply rotation matrix to each complex number
        corrected_real = real_part * cosine_phase - imaginary_part * sine_phase;
        corrected_imaginary = real_part * sine_phase + imaginary_part * cosine_phase;
    end
    
    % Validate results
    if any(~isfinite(corrected_real)) || any(~isfinite(corrected_imaginary))
        warning('Phase correction produced invalid values');
        corrected_real(~isfinite(corrected_real)) = 0;
        corrected_imaginary(~isfinite(corrected_imaginary)) = 0;
    end
    
    % Calculate correction statistics for user feedback
    original_magnitude = sqrt(real_part.^2 + imaginary_part.^2);
    corrected_magnitude = sqrt(corrected_real.^2 + corrected_imaginary.^2);
    
    magnitude_preservation = mean(corrected_magnitude ./ (original_magnitude + eps));
    phase_change_rms = sqrt(mean((atan2(corrected_imaginary, corrected_real) - ...
                                  atan2(imaginary_part, real_part)).^2));
    
    fprintf('  Magnitude preservation: %.6f (should be ~1.0)\n', magnitude_preservation);
    fprintf('  RMS phase change: %.6f radians\n', phase_change_rms);
    
    % Warn if magnitude is not preserved (indicates numerical issues)
    if abs(magnitude_preservation - 1.0) > 1e-10
        warning('Magnitude not preserved during phase correction (%.2e error)', ...
                abs(magnitude_preservation - 1.0));
    end
    
end