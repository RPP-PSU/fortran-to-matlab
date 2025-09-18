function create_sample_data()
% CREATE_SAMPLE_DATA Generates synthetic FT-IR interferogram data for testing
%
% This function creates sample interferogram files that can be used to test
% the FT-IR processing pipeline. The synthetic data simulates realistic
% FT-IR measurements with known spectral features.
%
% Output Files:
%   ifg_001.dat, ifg_002.dat, etc. - Synthetic interferogram data
%
% The generated data includes:
% - Simulated absorption bands at known frequencies
% - Realistic noise levels
% - Proper interferogram characteristics
%
% Author: Test data generation for MATLAB translation
% MATLAB Version: 2024 compatible

    fprintf('Creating Sample FT-IR Interferogram Data\n');
    fprintf('========================================\n');
    
    % Simulation parameters
    number_of_spectra = 5;           % Create 5 sample files for testing
    number_of_points = 8192;         % Data points per interferogram
    max_optical_path = 2.0;          % Maximum optical path difference (cm)
    noise_level = 0.05;              % Noise as fraction of signal
    
    % Simulated absorption bands (wavenumbers in cm^-1)
    absorption_bands = [3400, 2900, 1650, 1450, 1050];  % Typical organic bands
    band_intensities = [0.8, 0.6, 0.9, 0.5, 0.7];      % Relative intensities
    band_widths = [200, 150, 100, 80, 120];             % Band widths (cm^-1)
    
    fprintf('Generating %d interferogram files...\n', number_of_spectra);
    fprintf('Parameters:\n');
    fprintf('  Points per interferogram: %d\n', number_of_points);
    fprintf('  Max optical path difference: %.1f cm\n', max_optical_path);
    fprintf('  Noise level: %.1f%%\n', noise_level * 100);
    fprintf('  Absorption bands: %s cm^-1\n', mat2str(absorption_bands));
    
    % Generate optical path difference axis
    optical_path_difference = linspace(-max_optical_path, max_optical_path, number_of_points)';
    
    % Create sample data files
    for spectrum_index = 1:number_of_spectra
        
        % Initialize interferogram signal
        interferogram_signal = zeros(number_of_points, 1);
        
        % Add contribution from each absorption band
        for band_index = 1:length(absorption_bands)
            
            % Get band parameters
            wavenumber = absorption_bands(band_index);  % cm^-1
            intensity = band_intensities(band_index);
            width = band_widths(band_index);            % cm^-1
            
            % Add random variation between spectra (±10%)
            intensity_variation = 1 + 0.1 * (2 * rand() - 1);
            intensity = intensity * intensity_variation;
            
            % Calculate interferogram contribution for this band
            % Interferogram = Integral of spectrum * cos(2π * wavenumber * OPD)
            % For a Lorentzian band, this gives an exponentially decaying cosine
            
            decay_constant = 2 * pi * width;  % Related to band width
            cosine_term = cos(2 * pi * wavenumber * optical_path_difference);
            exponential_decay = exp(-decay_constant * abs(optical_path_difference));
            
            band_contribution = intensity * cosine_term .* exponential_decay;
            interferogram_signal = interferogram_signal + band_contribution;
        end
        
        % Add realistic noise
        noise = noise_level * randn(number_of_points, 1) * max(abs(interferogram_signal));
        interferogram_signal = interferogram_signal + noise;
        
        % Add small DC offset (common in real measurements)
        dc_offset = 0.1 * max(abs(interferogram_signal)) * (2 * rand() - 1);
        interferogram_signal = interferogram_signal + dc_offset;
        
        % Generate filename and save data
        filename = sprintf('ifg_%03d.dat', spectrum_index);
        
        % Combine optical path difference and signal for output
        output_data = [optical_path_difference, interferogram_signal];
        
        % Write to file in FORTRAN-compatible format
        try
            writematrix(output_data, filename, 'Delimiter', '\t', ...
                       'WriteMode', 'overwrite');
            
            % Verify file was created successfully
            if exist(filename, 'file')
                file_info = dir(filename);
                fprintf('  Created %s (%.1f KB)\n', filename, file_info.bytes / 1024);
            else
                fprintf('  Error: Failed to create %s\n', filename);
            end
            
        catch exception
            fprintf('  Error creating %s: %s\n', filename, exception.message);
        end
    end
    
    % Create a summary file with simulation parameters
    summary_filename = 'simulation_parameters.txt';
    summary_file = fopen(summary_filename, 'w');
    
    if summary_file ~= -1
        fprintf(summary_file, 'FT-IR Simulation Parameters\n');
        fprintf(summary_file, '===========================\n\n');
        fprintf(summary_file, 'Number of spectra: %d\n', number_of_spectra);
        fprintf(summary_file, 'Points per interferogram: %d\n', number_of_points);
        fprintf(summary_file, 'Max optical path difference: %.1f cm\n', max_optical_path);
        fprintf(summary_file, 'Noise level: %.1f%%\n', noise_level * 100);
        fprintf(summary_file, '\nSimulated Absorption Bands:\n');
        fprintf(summary_file, 'Wavenumber (cm^-1)\tIntensity\tWidth (cm^-1)\n');
        
        for i = 1:length(absorption_bands)
            fprintf(summary_file, '%.0f\t\t%.2f\t\t%.0f\n', ...
                    absorption_bands(i), band_intensities(i), band_widths(i));
        end
        
        fprintf(summary_file, '\nExpected Spectral Features:\n');
        fprintf(summary_file, '3400 cm^-1: O-H stretch (broad)\n');
        fprintf(summary_file, '2900 cm^-1: C-H stretch\n');
        fprintf(summary_file, '1650 cm^-1: C=C or C=O stretch\n');
        fprintf(summary_file, '1450 cm^-1: C-H bend\n');
        fprintf(summary_file, '1050 cm^-1: C-O stretch\n');
        
        fclose(summary_file);
        fprintf('  Created %s with simulation details\n', summary_filename);
    end
    
    fprintf('\nSample data creation complete!\n');
    fprintf('You can now run ftir_spectroscopy_processor() to test the translation.\n');
    fprintf('Expected output: spec_001.dat to spec_%03d.dat\n', number_of_spectra);

end