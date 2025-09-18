function ftir_spectroscopy_processor()
% FTIR_SPECTROSCOPY_PROCESSOR Main driver program for FT-IR data processing
%
% This function processes Fourier Transform Infrared (FT-IR) spectroscopy data
% by converting interferograms to absorption spectra using FFT algorithms.
% 
% Translated from FORTRAN to MATLAB with enhanced functionality and readability.
% 
% Key improvements from original FORTRAN:
% - Descriptive variable names for better code clarity
% - Modular organization with separate functions for each operation
% - MATLAB vectorized operations for improved performance
% - Enhanced error handling and user feedback
% - Compatibility with MATLAB 2024
%
% Processing Steps:
% 1. Generate frequency axis based on spectral parameters
% 2. Read interferogram data from input files
% 3. Apply apodization window to reduce spectral artifacts
% 4. Perform Fast Fourier Transform (FFT)
% 5. Apply phase correction for proper spectrum phasing
% 6. Calculate magnitude spectrum from complex FFT result
% 7. Save processed spectra to output files
%
% Author: Translated from FORTRAN for FT-IR spectroscopy applications
% Date: 2024
% MATLAB Version: 2024 compatible

    % Define processing parameters with descriptive names
    number_of_data_points = 8192;      % Total data points in interferogram
    number_of_spectra = 5;             % Total number of spectra to process (updated for demo)
    starting_wavenumber = 4000.0;      % Starting frequency in cm^-1
    ending_wavenumber = 400.0;         % Ending frequency in cm^-1
    spectral_resolution = 4.0;         % Spectral resolution in cm^-1
    phase_correction_angle = 0.0;      % Phase correction in radians
    
    % Display program header with processing information
    fprintf('FT-IR Spectroscopy Data Processing Program\n');
    fprintf('==========================================\n');
    fprintf('Processing %d spectra with %d data points each\n', ...
            number_of_spectra, number_of_data_points);
    fprintf('Spectral range: %.1f - %.1f cm^-1\n', ...
            starting_wavenumber, ending_wavenumber);
    fprintf('Resolution: %.1f cm^-1\n', spectral_resolution);
    fprintf('\n');
    
    % Generate frequency axis for the spectra
    frequency_axis = generate_frequency_axis(starting_wavenumber, ...
                                           ending_wavenumber, ...
                                           number_of_data_points);
    
    % Initialize progress tracking
    successful_spectra = 0;
    failed_spectra = 0;
    
    % Start timing for performance monitoring
    processing_start_time = tic;
    
    % Main processing loop for all spectra
    fprintf('Processing spectra:\n');
    for spectrum_index = 1:number_of_spectra
        
        % Generate input filename for current interferogram
        interferogram_filename = sprintf('ifg_%03d.dat', spectrum_index);
        
        % Attempt to read interferogram data
        [optical_path_difference, interferogram_signal, read_success] = ...
            read_interferogram_data(interferogram_filename, number_of_data_points);
        
        % Skip processing if file read failed
        if ~read_success
            failed_spectra = failed_spectra + 1;
            fprintf('  Spectrum %d: Failed to read %s\n', ...
                    spectrum_index, interferogram_filename);
            continue;
        end
        
        % Apply apodization window to reduce spectral artifacts
        apodized_signal = apply_apodization_window(interferogram_signal, 'happ-genzel');
        
        % Perform Fast Fourier Transform to convert to frequency domain
        [real_part, imaginary_part] = perform_fft_transform(apodized_signal);
        
        % Apply phase correction to align spectrum properly
        [phase_corrected_real, phase_corrected_imag] = ...
            apply_phase_correction(real_part, imaginary_part, phase_correction_angle);
        
        % Calculate magnitude spectrum from complex FFT result
        absorption_spectrum = calculate_magnitude_spectrum(phase_corrected_real, ...
                                                         phase_corrected_imag);
        
        % Generate output filename and save processed spectrum
        spectrum_filename = sprintf('spec_%03d.dat', spectrum_index);
        write_spectrum_data(spectrum_filename, frequency_axis, absorption_spectrum);
        
        successful_spectra = successful_spectra + 1;
        
        % Display progress every 10 spectra
        if mod(spectrum_index, 10) == 0
            fprintf('  Processed %d/%d spectra\n', spectrum_index, number_of_spectra);
        end
    end
    
    % Display final processing summary
    total_processing_time = toc(processing_start_time);
    fprintf('\nProcessing Complete!\n');
    fprintf('==================\n');
    fprintf('Successfully processed: %d spectra\n', successful_spectra);
    fprintf('Failed to process: %d spectra\n', failed_spectra);
    fprintf('Total processing time: %.2f seconds\n', total_processing_time);
    
end