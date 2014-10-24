function getsolardata(startdate,enddate,TZ,site)

% Creates TFVMetcsv files for a pair of BoM sites
% Ensure that the data extraction scripts have been used and the .mat files
% exist in the relevant path folder. All calculations are hourly. 
% "remove_nans" is used to remove any nans in the data. Typical input -
% startdate = datenum(2001,01,01,09,00,00);
% enddate = datenum(2002,01,01,09,00,00);
% site = 'airport';
% TZ = 7.5;
% IMPORTANT - Comment out the file writing part to just generate the plots.

load metdata.mat;

met = metdata.(site);

tt = find(met.Date >= startdate & met.Date <= enddate);

% Get Bird Model Data

lat = met.lat;
lon = met.lon;
[GHI ZenithAngle NewDate] = genBirdSolarData(lat,lon,startdate,enddate,TZ);
met.GHI = GHI;

% Calculate the model data based on the model correlation

for i = 1:length(met.TC_interp)

    if met.TC_interp(i) >= 0.02
        met.Rad_Model(i) = (0.66182* power(met.TC_interp(i),2) - 1.5236*met.TC_interp(i) + 0.98475) * met.GHI(i);
    else
        met.Rad_Model(i) = met.GHI(i);
    end
end


