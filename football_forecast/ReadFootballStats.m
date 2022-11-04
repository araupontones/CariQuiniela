% Build URL string
url_string = 'http://www.football-data.co.uk/mmz4281/1213/SP1.csv';

% Open a connection to the URL and retrieve data into a buffer
buffer        = java.io.BufferedReader(...
              java.io.InputStreamReader(...
              openStream(...
              java.net.URL(url_string))));


% Read the first line (a header) and discard
dummy         = readLine(buffer);

ptr = 1;

% Read all remaining lines in buffer
while 1
    
    % Read line
    buff_line = char(readLine(buffer)); 
    
    % Break if this is the end
    if length(buff_line)<3, break; end
    
    % Find comma delimiter locations
    commas    = find(buff_line == ',');
    
    % Extract high, low, open, close, etc. from string
    Date      = buff_line(5:commas(2)-1);
    HTeam     = buff_line(commas(2)+1:commas(3)-1);
    ATeam     = buff_line(commas(3)+1:commas(4)-1);
    fthg      = str2num(buff_line(commas(4)+1:commas(5)-1));
    ftag      = str2num(buff_line(commas(5)+1:commas(6)-1));
    
    DATE{ptr,1}   = Date;
    HTEAM {ptr,1} = HTeam;
    ATEAM {ptr,1} = ATeam;
    FTHG (ptr,1)  = fthg;
    FTAG (ptr,1)  = ftag;
    
    ptr       = ptr + 1;
    
end