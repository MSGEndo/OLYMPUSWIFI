unit OlympusShare;
{ Summary: This unit allows the user to download images from a wireless enabled Olympus camera, leaving the originals on the SD card.

  OS:      Windows 10 (working well), Ubuntu 18.04 (not working yet), Mac OS not tested
  DevTool: Lazarus2.06/FPC3.04
  Camera: Olympus OM-D E-M10
  How it works: Main class TOiShareReader, inherted from a TfpHTTPClient component, manages connecion to the camera oer a wireless signal.
                The TfpHTTPClient is an component which is not installed in Lazarus by default and needs to be installed with
                Menu|Package!Install/Uninstall Packages.

  Why develop this program?  Olympus makes Android and iOS apps (Oi.Share) to allow WiFi download of images from their camera but do
  not have an app for PC, Mac or Linux desktops. So I have made this one to use. Also the phone apps do not allow automated download on the fly like
  the old EyeFi SD cards did, sadly now not manufactured anymore.

  How to use  How to use this program

  1.  Take some photos on your Olympus camera
  2.  Use the Olympus camera menu to select Connection to Smartphone.  This starts the camera WiFi server.
  3.  Connect your computer to this WiFi signal in the usual way for your operating system
  4.  Run this OlympusWIFI program
  5.  Set the download directory as required ("Transfer Files to This Directory") where  from the will be transferred
  6.  Option 1: Press the grey triangle once and wait for images on the SD card to be transferred to your computer's download directory
  7.  Option 2: Click the Repeat Timer checkbox and wait for the timer (e.g. 60s) to automate image transfers periodically.
  8.  Downloaded files will be remembered, so will not be downloaded again on later transfers.

  Enjoy

  Martin Gale ( 04/01/2020 Copyright.)
  mail@isendo.com.au
  This program is open source and can be used for commercial and non commercial purposes (MIT licence) so long as
  full acknowledgement is made of the author's original contribution in any distribution of the derived work. The software is as is,
  and no warranty of suitability of purpose or efficacy and is given, and the authors accept no liability whatsoever for its use, consequences or
  effects.   If it is not suitable for your  purpose then do not use this software.  Otherwise, please make good use of it.

Notes:
i)   A record of the downloaded file names and other details are recorded in a file ..OlympusCameraDownloadRecord.txt.  This is delimited with
the string '$!?' and is queried with string.contains for previous entries on deciding if to download a file or not.
ii) Having to switch WiFi signals on the computer to connect to the camera then back to the usually home router is a pain.  Need to automate this.

See comments in the code for further explanations

Enjoy

Martin Gale
mail@isendo.com.au

// TODO: There is still a very small memory leak somewhere of about $50k per access to the server - find this and fix it.
// TODO: Implement only downloading selected filetypes
// TODO: Work out how to automatically seek and connect to the WiFi SSID signal even though another wifi signal may previously be connected - on Win and Linux
// TODO: Check this works on Linux too - some more IFDEFs needed probably for any file paths
// TODO: Release as open source on github and promote in Panasonic Lumix and Olympus user lists
}
  {$mode delphi}{$H+}
interface

uses
  Classes, SysUtils, ExtCtrls, Dialogs, fphttpclient
   {$IFDEF WINDOWS}
  ,win32proc   // Allows OSVersion to be defined for different Windows versions
   {$ENDIF}
  ;

Type
TImageFileInfo = record  // A single record of data for files or directories
  APath,
  AFileName,   // NB The Directory name is held in the AFilename field for FDirList
  AUnknown,
  AFileSize,
  ADate,
  ATime: String;
  ADownloaded: boolean;
end;

 TDCIMList  = array of TImageFileInfo; // An array of records for holding data about files or directories
 TDCIMLists = array of TDCIMList;      // An array of TDCIMList arrays for holding data about multiple dirctories of images
 ListTypes  = Set of (APath, AFileName, AUnknown, AFileSize, ADate, ATime, ADownloaded);  // Nominals for data types in TImageFileInfo

 { TTransferImages }

Type

 //TOnUpdateGUIEvent = procedure(PercentDone: integer) of Object;    // an event for the TThread to be assigned to a procedure in the main form of the aplication

 TTransferImages = class(TThread) // The actual thread class for one event of accessing the Olympus camera server.  Using a TThread to free up the GUI
private
 FHTTPClient:       TfpHTTPClient;       // the http client component used to contact the server
 FHTTPRequest:      String;              // the string containing the request to the HTTP server
 FHTTPResponseSL:   TStringlist;         // the response in TStringList format
 FHTTPResponseMS:   TMemoryStream;       // the response in TMemoryStream format
 FResponseFilename: String;              // the path and filename for where to save the TStringlist or TMemoryStream response received from the HTTP server
 FErrorList:        TStringlist;         // holds a stringlist of any error messages encountered to be passed on to the user
 FOutputSort:      (oStrList, oMStream); // whether the thread asks for a TStringlist or a TMemoryStream from the server
public
 Constructor Create;
 Destructor  Destroy;  override;
 Function    GetHTTPResponseSL(AHTTPRequest: String; AResponseFilename: String): TStringlist;   // outputs a TStringlist from the server data
 Function    GetHTTPResponseMS(AHTTPRequest: String; AResponseFilename: String): TMemoryStream; // outputs a TMemoryStream from the server data
 procedure   Execute; override;                                                                 // Calls the seever
 end;


{ TOIShareReader }
Type
 TOIShareReader = class // The class which the uses the class TTransferImages classs to set up and receive data from the Olympus camera SD card server.

private
 FServerAddr:       string;     // The TfpHTTPClient URL to reach the camera server - typically for Olympus cameras 'http://oishare' with '/DCIM' added to access the image root directory
 FDCIMDir:          string;     // The SD card root directory for images - typically '/DCIM'
 FDirList:          TDCIMList;  // An array of folder information in the SD card root /DCIM directory
 FImageLists:       TDCIMLists; // A set of arrays of image information, one array for one SD card directory
 FDownLoadDir:      string;     // Stores the destination directory for images on download
 FDownloadedList:   TStringList;// A list of all the previous image downloads done - referenced so previous images are not downloaded again
 FLastDownloadTime: TDateTime;  // Time of last download
 FLastDownLoaded:   string;     // The path and filename for the last downloaded image

 Procedure   Initialize;
 Procedure   CleanUp;
 Function    FindDownloadRecord: integer;
 Function    GetOSVersion: String;
 Function    GetDCIMResponseSL(AHTTPRequest: String; AResponseFilename: String): TStringlist;     // Returns a TStringlist
 Function    GetDCIMResponseMS(AHTTPRequest: String; AResponseFilename: String): TMemoryStream;   // Returns a TMemoryStream
 Function    GetDCIMList(AHTTPResponse: TStringList): TDCIMList;                                  // Generic function to turn the Javascript response into an array of file data
 Function    GetDCIMDirList(AServerURL: String): TDCIMList;                                       // Specific response to use GetDCIMDirList to retrieve SD card directory data
 Function    GetDCIMImageList(ADir: String): TDCIMList;                                           // Specific response to use GetDCIMDirList to retrieve SD card image file data

public
 ErrorList:  TStringList;   // Holds a listing of any errors encountered in the program.
 IsDownloading: boolean;    // Prevents further attepts at server contact when server already connected
 TransferNow: TTransferImages;
 Constructor Create(AOwner: TComponent);
 Destructor  Destroy; override;
 Procedure   RegisterAllSDCardFilesAsDownloaded;
 Function    CountFilesForDownload: integer;
 Function    DCIMListToStringList(ADCIMList: TDCIMList; AListTypes: ListTypes): TStringList;
 Procedure   GetSDCardData;
 Procedure   DownloadImages(SaveImageDir: String);
 Function    BeautifyDownloadList: TStringList;
 Procedure   RememberDownloadDir;

Published
 property ServerAddr:       string      read FServerAddr       write FServerAddr;        // typically for Olympus camera 'http://oishare'
 property DCIMDir:          string      read FDCIMDir          write FDCIMDir;           // typically '/DCIM'
 property DirList:          TDCIMList   read FDirList          write FDirList;           // As above in private section
 property ImageLists:       TDCIMLists  read FImageLists       write FImageLists;        //                   "
 property DownloadDir:      String      read FDownloadDir      write FDownloadDir;       //                   "
 property DownloadList:     TStringList read FDownloadedList   write FDownloadedList;    //                   "
 property LastDownLoadTime: TDateTime   read FLastDownLoadTime write FLastDownLoadTime;  //                   "
 property LastDownlowdedImage: String   read FLastDownloaded   write FLastDownloaded;    //                   ?

end;

implementation

 uses Olympus_ImageSave1;

{ TTransferImages } // TThread for the actual server communication so GUI is not locked and can be updated

constructor TTransferImages.Create;
begin
  inherited Create(false);   // false indicates the thread is not suspended, so it begins to run as soon as the constructor is finished
  FreeOnTerminate := false;  // Need to access info first, before free the TThread so dont free on terminate
  FHTTPClient     := TfpHTTPClient.create(nil);
  FHTTPResponseSL := TStringlist.create;
  FHTTPResponseMS := TMemoryStream.create;
  FErrorList      := TStringlist.create;
  FOutputSort     := oStrList;                // default is a stringlist
end;

destructor TTransferImages.Destroy;
begin
  //FHTTPClient.free;      // These objects should be freed before the TThread is freed but if do so then access violation so
  //FHTTPResponseSL.free;  // it seems that the TThread class destroy method frees them later anyway.
  //FHTTPResponseMS.free;
  //FErrorList.free;
  inherited destroy;
end;

function TTransferImages.GetHTTPResponseSL(AHTTPRequest: String; AResponseFilename: String): TStringlist;
begin   // Arranges to GET a TStringlist from the server, with the variable FOutputSort flagging the output is to be a stringlist
  FOutputSort       := oStrList;
  FHTTPRequest      := AHTTPRequest;
  FResponseFilename := AResponseFilename;
  Execute;
  Result            := FHTTPResponseSL;
end;

function TTransferImages.GetHTTPResponseMS(AHTTPRequest: String; AResponseFilename: String): TMemoryStream;
begin   // Arranges to GET a TMemoyStream from the server, with the variable FOutputSort flagging the output is to be a memorystream
  FOutputSort       := oMStream;
  FHTTPRequest      := AHTTPRequest;
  FResponseFilename := AResponseFilename;
  Execute;
  Result            := FHTTPResponseMS;
end;

procedure TTransferImages.Execute;
begin   // THis uses TfpHTTPClient to get from the server either a TStringlist of text or else a TMemorystream of data depending on the
        // FOuptutSource flag setting. If there is a valid directory defined by FResponseFilename, then the file either as text or data,
        // will be saved to this given directory

 If (FHTTPClient  <> nil)                      and
    (FHTTPRequest <> '')                       and
    (FHTTPRequest.Contains('http://oishare'))  then  // Usually 'http://oishare' for olympus cameras
 Try
   If FOutputSort = oStrList then FHTTPClient.SimpleGet(FHTTPRequest, FHTTPResponseSL) else
   If FOutputSort = oMStream then FHTTPClient.SimpleGet(FHTTPRequest, FHTTPResponseMS);

   // Save server response file to disk only if a filename with valid path has been given
   If   (FResponseFilename > '') and
        (DirectoryExists(ExtractFilePath(FResponseFilename))) then
     Try
       If FOutputSort = oStrList then FHTTPResponseSL.SaveToFile(FResponseFilename) else
       If FOutputSort = oMStream then FHTTPResponseMS.SaveToFile(FResponseFilename);
     except
       FErrorList.add('Sorry: Could not save file ' + FResponseFilename + ' [' + DateTimeToStr(Now,False) + ']');
     end;
 except;
   //beep;
   If FOutputSort = oStrList then FHTTPResponseSL.clear else
   If FOutputSort = oMStream then FHTTPResponseMS.clear;
   FErrorList.add('Sorry: The Olympus camera could not be reached at ' + FHTTPRequest + ' [' + DateTimeToStr(Now,False) + ']');
 end;
end;

{ TOIShareReader }

constructor TOIShareReader.Create(AOwner: TComponent);
begin
 //inherited Create(AOwner);
 Initialize;
end;

destructor TOIShareReader.Destroy;
begin
  CleanUp;
  inherited destroy;
end;

function TOIShareReader.GetOSVersion: String;  // Works out which operating system so can decide whether to use / or \ in paths
                                               // From https://forum.lazarus.freepascal.org/index.php?topic=15390.0   ( Thanks jwdietrich! )
begin
  {$IFDEF LCLcarbon}
  GetOSVersion := 'MacOS.';
  {$ELSE}
    {$IFDEF Linux}
    GetOSVersion := 'Linux';
    {$ELSE}
      {$IFDEF UNIX}
       Get OSVersion := 'Unix ';
      {$ELSE}
        {$IFDEF WINDOWS}
        // Need to have Win32Proc unit in the uses clause for this to work
        If WindowsVersion       = wv95         then GetOSVersion := 'Windows 95 '
         else if WindowsVersion = wvNT4        then GetOSVersion := 'Windows NT v.4 '
         else if WindowsVersion = wv98         then GetOSVersion := 'Windows 98 '
         else if WindowsVersion = wvMe         then GetOSVersion := 'Windows ME '
         else if WindowsVersion = wv2000       then GetOSVersion := 'Windows 2000 '
         else if WindowsVersion = wvXP         then GetOSVersion := 'Windows XP '
         else if WindowsVersion = wvServer2003 then GetOSVersion := 'Windows Server 2003 '
         else if WindowsVersion = wvVista      then GetOSVersion := 'Windows Vista '
         else if WindowsVersion = wv7          then GetOSVersion := 'Windows 7 '
         else if WindowsVersion = wv8          then GetOSVersion := 'Windows 8 '      // MG added to update the code
         else if WindowsVersion = wv8_1        then GetOSVersion := 'Windows 8.1 '    //          "
         else if WindowsVersion = wv10         then GetOSVersion := 'Windows 10 '     //          "
         else if WindowsVersion = wvLater      then GetOSVersion := 'Windows >10 '    //          "
         // what about windows 10?
         else                                       GetOSVersion := 'Windows ';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

procedure TOIShareReader.Initialize;     // Sets up the program initial default state
var
  a, Found: integer;
begin
  FServerAddr       := 'http://oishare'; // 'http://oishare' typically for Olympus camera. 'http://oishare/DCIM' to access files (no need for /*.*)
  FDCIMDir          := '/DCIM';          // typically '/DCIM'
  SetLength(FDirList,0);
  SetLength(FImageLists,0);
  ErrorList         := TStringList.create;
  //{$IFDEF WINDOWS}
  FDownLoadDir      := GetCurrentDir;
  //{$ELSE}
  FDownLoadDir      := GetCurrentDir;
  //{$ENDIF}
  FDownloadedList   := TStringList.create;
  FLastDownloadTime := Now;
  FLastDownloaded   := '';
  IsDownloading     := false;
  Try
    {$IFDEF WINDOWS}
    FDownloadedList.LoadFromFile(GetCurrentDir + '\OlympusCameraDownloadRecord.txt');
    {$ELSE}
    FDownloadedList.LoadFromFile(GetCurrentDir + '/OlympusCameraDownloadRecord.txt');
    {$ENDIF}
  Except;
    FDownloadedList := TStringList.create;
  end;
    a := FindDownloadRecord;
    If a > -1 then
    FDownLoadDir := Copy(FDownloadedList.strings[a],13,length(FDownloadedList.strings[a])-12);
    If not DirectoryExists(FDownLoadDir) then FDownloadDir := GetCurrentDir;
end;

procedure TOIShareReader.CleanUp;        // Organises the final program state
var
  a: integer;
begin
  SetLength(FDirList,0);
  For a := 0 to length(FImageLists) -1 do
  SetLength(FImageLists[a],0);
  SetLength(FImageLists,0);
  FDownloadedList.SaveToFile(GetCurrentDir + '\OlympusCameraDownloadRecord.txt');
  FreeAndNil(FDownloadedList);
  FreeAndNil(ErrorList);
end;

Function TOIShareReader.FindDownloadRecord: integer;
var  // Identifies where the download directory record is in the OlympusCameraDownloadRecord.txt file
  a, Found: integer;        // All this so I dont use TIniFile which is not available on Linux!
begin
  Result := -1;
  If (FDownloadedList <> nil)    and
     (FDownloadedList.count > 0) then
  begin;
    a     := 0;
    Found := -1;
    Repeat
      If FDownloadedList[a].contains('DownloadDir=') then
      Found := a;
      inc(a);
    until (Found > -1)                or
          (a >= FDownloadedList.Count -1);
  end;
  Result := Found;
end;

function TOIShareReader.GetDCIMResponseSL(AHTTPRequest: String; AResponseFilename: String): TStringlist;// Queries the Olympus camera http server for a response
var
  AHTTPThread: TTransferImages;
begin;
  AHTTPThread := TTransferImages.create;
  Result      := AHTTPThread.GetHTTPResponseSL(AHTTPRequest,AResponseFilename);
  ErrorList.AddStrings(AHTTPThread.FErrorList);
  AHTTPThread.Free;
end;

function TOIShareReader.GetDCIMResponseMS(AHTTPRequest: String;  AResponseFilename: String): TMemoryStream; // Queries the Olympus camera http server for a response
var
  AHTTPThread: TTransferImages;
begin;
  AHTTPThread := TTransferImages.create;
  Result      := AHTTPThread.GetHTTPResponseMS(AHTTPRequest,AResponseFilename);
  ErrorList.AddStrings(AHTTPThread.FErrorList);
  AHTTPThread.Free;
end;

function TOIShareReader.GetDCIMList(AHTTPResponse: TStringList): TDCIMList;  // Parses the wireless Olympus camera http server http response for directory or file info in included Javascript object
var                                                                          // NB: Need to specify System.delete for text deletions otherwaise calls TFPHTTPClient.delete instead
 ts1: string;
 a, b, S1, S2: integer;
begin
  Setlength(Result,0);
  If (AHTTPResponse <> nil)    and
     (AHTTPResponse.Count > 0) and
     (AHTTPResponse.Text.Contains('wlansd = new Array();')) then  // ie. The HTTPresponse contains a javascript array object called wlansd
  For a := 0 to AHTTPResponse.count -1 do
  begin;
    If (copy(AHTTPResponse[a],1,7) = 'wlansd[') and
       (AHTTPResponse[a].Contains('=')) then       // The Javascript wlansd object elements lines each begins with 'wlansd[' and has an '=' in it
    begin;                                         // i.e. this line is a Javascript wlansd object array element representing one image file
      Setlength(Result, length(Result) +1);
      b   := length(Result) -1;
      ts1 := AHTTPResponse[a];
      S1  := pos('=', ts1) + 2;                    // locates the second character after the '=' sign in each wlansd object array line
      If (S1 > 0)            and
         (S1 <= length(ts1)) then
      begin;
        ts1 := copy(ts1, S1, length(ts1) - S1 -1); // grabs the quoted string including six comma separated data values, also not including the trailing ;
        // remove the two " quotation characters
        If pos('"', ts1) > 0 then System.delete(ts1, pos('"', ts1), 1);
        If pos('"', ts1) > 0 then System.delete(ts1, pos('"', ts1), 1);

        // Now parse the javascript in the server response for the six comma separated data for each image file in this array

        // APath
        S2 := Pos(',',ts1) ;
        If S2 > 0 then
        begin;
          Result[b].APath := copy(ts1, 1, S2 -1); // -1 so removes the , at the end of the http get path call
          System.Delete(ts1,1,S2);                // deletes commma    // NB: The path name does not include the server name 'http://oishare',
                                                  // and uses unix '/' path delimiters.  The path sometimes includes an initial and trailing '/'
          If pos('/*.*', Result[b].APath) > 0 then
          System.delete(Result[b].APath, pos('/*.*', Result[b].APath), 4) else // Gets rid of all of /*.* if present on the path string
          If Result[b].APath[length(Result[b].APath)] = '/' then
          System.delete(Result[b].APath, length(Result[b].APath), 1)   ;       // or gets rid of just / if present on path string
          // i.e the path has no trailing forward slash or *.* any more. But actually this works on requesting the file list from the server. Dont need /*.*
        end;

        //AFilename
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          Result[b].AFileName := copy(ts1, 1, S2-1);
          System.Delete(ts1,1,S2);
        end;

        //AFileSize
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          Result[b].AFileSize := copy(ts1, 1, S2-1);
          System.Delete(ts1,1,S2);
        end;

        //AUnknown - not sure what this represents but it seems to be always 0
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          Result[b].AUnknown := copy(ts1, 1, S2-1);
          System.Delete(ts1,1,S2);
        end;

        //ADate      // Have assumed this integer value is the date - not sure of the format yet
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          Result[b].ADate := copy(ts1, 1, S2-1);
          System.Delete(ts1,1,S2);
        end;

        //ATime     // Have assumed this integer value is the timestamp - not sure of the format yet
        S2 := Pos(',',ts1);
        If (S2 = 0) and (length(ts1) > 0) then    // ie no commas left
        begin;
          Result[b].ATime := copy(ts1, 1, length(ts1));
          //System.Delete(ts1,1,S2);  // No terminal comma to delete here
        end;
        // Have used the field delimiter '$!?' as this is unlikely to be in any directory or filename
        // Downloaded already?           // format    /DCIM/101OLYMP$!?ZC110024.JPG$!?20363$!?32035$!?-1
        If FDownloadedList.Text.Contains
        (Result[b].APath + '$!?' + Result[b].AFileName + '$!?' + Result[b].ADate + '$!?' + Result[b].ATime) then
           Result[b].ADownloaded := true else
           Result[b].ADownloaded := false;
      end;
    end;
  end;
end;

procedure TOIShareReader.RememberDownloadDir;
var
  a: integer;
begin
    // Update the saved download directory to file too



  a := FindDownloadRecord;
  If a > -1 then
  FDownloadedList[a] :=    'DownloadDir=' + FDownloadDir  else
  FDownloadedList.Insert(0,'DownloadDir=' + FDownloadDir);
end;

procedure TOIShareReader.RegisterAllSDCardFilesAsDownloaded;
var            // Checks the SD card for files and marks them all as previously downloaded
 a,b: integer; // This is useful if you start using an SD card from another camera which has many previous images on it
begin;         // which have not been downloaded by this program, and you do not wish to download these historical images now
  GetSDCardData;
  If length(FImageLists) > 0 then
  For a := 0 to length(FImageLists) -1 do
  begin;
    If (length(FImageLists[a]) > 0) then
    For b := 0 to length(FImageLists[a]) -1 do
    begin
      FImageLists[a].[b].ADownloaded := true;
      If not FDownloadedList.Text.Contains(
             FImageLists[a].[b].APath + '$!?' + FImageLists[a].[b].AFileName + '$!?' +
             FImageLists[a].[b].ADate + '$!?' + FImageLists[a].[b].ATime) then
      FDownloadedList.add(
             FImageLists[a].[b].APath + '$!?' + FImageLists[a].[b].AFileName + '$!?' +
             FImageLists[a].[b].ADate + '$!?' + FImageLists[a].[b].ATime     + '$!?' + BoolToStr(FImageLists[a].[b].ADownloaded));
    end;
  end;
  RememberDownloadDir;
  {$IFDEF WINDOWS}
  Try FDownloadedList.SaveToFile(GetCurrentDir + '\OlympusCameraDownloadRecord.txt');
  except
    beep;
    ErrorList.add('Sorry: Could not save the list of downloaded files at: ' + GetCurrentDir + '\OlympusCameraDownloadRecord.txt' + ' [' + DateTimeToStr(Now,False) + ']');
  end;
  {$ELSE}
  FDownloadedList.SaveToFile(GetCurrentDir + '/OlympusCameraDownloadRecord.txt');
  {$ENDIF}
end;


function TOIShareReader.CountFilesForDownload: integer; // Counts how many images need downloading
var             // Updates and tallies the number of image files on the SD card due for downoad
 a, b: integer; // Make sure you run GetSDCardData first to have a fresh file listing
begin;
  Result := 0;
  For a := 0 to length(FImageLists) -1 do
  begin;
    For b := 0 to length(FImageLists[a]) -1 do
    begin;
      If FDownloadedList.Text.Contains(  // checks the download records again to be sure, and updates ADownloaded field
         FImageLists[a].[b].APath + '$!?' + FImageLists[a].[b].AFileName + '$!?' +
         FImageLists[a].[b].ADate + '$!?' + FImageLists[a].[b].ATime) then
         FImageLists[a].[b].ADownloaded := true else
         FImageLists[a].[b].ADownloaded := false;
      If FImageLists[a].[b].ADownloaded = false then
      Inc(Result);
    end;
  end;
end;

function TOIShareReader.GetDCIMDirList(AServerURL: String): TDCIMList; // Queries the server for a list of directories on the SD card
var
 AHTTPResponse: TStringList;
Begin;
  FServerAddr := AServerURL;
  Setlength(Result,0);                                             // AServerURL
  AHTTPResponse := GetDCIMResponseSL(FServerAddr + FDCIMDir,'');   //'http://oishare/DCIM','');
  Result        := GetDCIMList(AHTTPResponse);                     // NB This TDCIMList needs to be freed externally - don't forget
  FreeAndNil(AHTTPResponse);
end;

function TOIShareReader.GetDCIMImageList(ADir: String): TDCIMList; // Queries the server for a list of directories on the SD card
var
  AHTTPResponse: TStringList;
begin
  Setlength(Result,0);
  If (length(ADir) > 0) then
  begin;
    If not (ADir[1] ='/') then
    insert('/',ADir,1); // Adds a prefix '/' if not present already in ADir FServerAddr + FDCIMDir
    AHTTPResponse := GetDCIMResponseSL(FServerAddr + FDCIMDir + ADir,'');  //'http://oishare/DCIM' + /directory
    Result        := GetDCIMList(AHTTPResponse);
    //FImageList    := GetDCIMList(AHTTPResponse);  // Do we want/need this to happen
    FreeAndNil(AHTTPResponse);
  end else
  ErrorList.add('Sorry: No image directory name was given to read' + ' [' + DateTimeToStr(Now,False) + ']'); ;
end;

function TOIShareReader.DCIMListToStringList(ADCIMList: TDCIMList; AListTypes: ListTypes): TStringList;
         // Converts a DCIMList to a stringlist for use in GUI or for saving to file
var      // Listtypes allows you to specify which data fields to include in the stringlist output
  a: integer;
  ts1: string;
begin
  Result := TStringList.create;  // NB: Remember to free this externally - don't forget
  If (length(ADCIMList) > 0) and
     (AListTypes <> [])      then
  For a := 0 to length(ADCIMList) -1 do
  begin;
    ts1 := '';
    If APath       in AListTypes then ts1 := ts1 + ', ' + ADCIMList[a].APath;
    If AFileName   in AListTypes then ts1 := ts1 + ', ' + ADCIMList[a].AFileName;
    If AFileSize   in AListTypes then ts1 := ts1 + ', ' + ADCIMList[a].AFileSize;
    If AUnknown    in AListTypes then ts1 := ts1 + ', ' + ADCIMList[a].AUnknown;
    If ADate       in AListTypes then ts1 := ts1 + ', ' + ADCIMList[a].ADate;
    If ATime       in AListTypes then ts1 := ts1 + ', ' + ADCIMList[a].ATime;
    If ADownloaded in AListTypes then ts1 := ts1 + ', ' + BoolToStr(ADCIMList[a].ADownloaded,'Downloaded', 'Not Downloaded');
    If ts1 <> '' then System.delete(ts1,1,2);  // Remove unnecessary leading ', '
    Result.add(ts1);
  end else
  Result.text := 'No Data';
end;

procedure TOIShareReader.GetSDCardData; // Queries the SD card for a list of directories or data
var
  a: integer;
begin;
  //clear all previous lists to avoid memory leaks
  SetLength(FDirList,0);
  IsDownloading := true;
  For a := 0 to length(FImageLists) -1 do
  SetLength(FImageLists[a],0);
  SetLength(FImageLists,0);
  // Then read new SD card directory list FServerAddr  + FDCIMDir
  FDirList := GetDCIMDirList(FServerAddr); // NB: the '/DCIM' is added later not here
  // Then read each directory's image list
  If length(FDirList) > 0 then
  begin;
    Setlength(FImageLists,length(FDirList));  // Make one image list for each directory
    For a := 0 to length(FImageLists) -1 do
    begin;
      FImageLists[a] := GetDCIMImageList(FDirList[a].AFileName); // Gets each image directory's file list for display
    end
 end;
 IsDownloading := false;
end;

procedure TOIShareReader.DownloadImages(SaveImageDir: String);
                        // Downloads any images found on the SD card which have not yet been downloaded
var                     // a is a directory iterator, b is an image interator
  a, b, c, d: integer;  // c is a counter of how many files downloaded at any point in time. d is the total number of files to be downloaded
  AHTTPRequest: string;
  FStream: TMemoryStream;
begin
  IsDownloading := true;
  If not DirectoryExists(SaveImageDir) then Try MkDir(SaveImageDir); Except; end;   // Tries to mke the receiving download dir if not already present
  If DirectoryExists(SaveImageDir) then
  begin;
    d := CountFilesForDownload;  // Also updates SDCard list of directories and files
    c := 0;
    If (length(FImageLists) > 0) and (d > 0) then
    For a := 0 to length(FImageLists) -1 do
    begin;
    If IsDownloading = false then break;       // allows download escape from For loop if user wishes
      If length(FImagelists[a]) > 0 then
      begin;
        For b := 0 to length(FImagelists[a]) -1 do
        begin;
          If IsDownloading = false then break; // allows download escape from For loop if user wishes
          If not FImageLists[a].[b].ADownloaded then
          begin;
            AHTTPRequest :=  FServerAddr + FImageLists[a].[b].APath + '/' + FImageLists[a].[b].AFileName;
            {$IFDEF WINDOWS}
            FStream := GetDCIMResponseMS(AHTTPRequest, SaveImageDir + '\' + FImageLists[a].[b].AFileName); // Queries the Olympus camera http server for a response, saves result to file
            {$ELSE}
            FStream := GetDCIMResponseMS(AHTTPRequest, SaveImageDir + '/' + FImageLists[a].[b].AFileName); // Queries the Olympus camera http server for a response, saves result to file
            // If Linux or Mac need a / not a \ in the path.
            {$ENDIF}
            FreeAndNil(FStream);
            FImageLists[a].[b].ADownloaded := true;
            //This records with a delimited string whether a file has been donwloaded before or not.
            // The string '$!?' is the delimiter - unikely this will be naturally in a file name

            FDownloadedList.add(FImageLists[a].[b].APath + '$!?' + FImageLists[a].[b].AFileName + '$!?' +
                                FImageLists[a].[b].ADate + '$!?' + FImageLists[a].[b].ATime     + '$!?' + BoolToStr(FImageLists[a].[b].ADownloaded));

            {$IFDEF WINDOWS}   // Cross platform hassle fixed
            FLastDownloaded := Self.DownloadDir + '\' + FImageLists[a].[b].AFileName;   // records the path and filename of the last downloaded image for display
            {$ELSE}
            FLastDownloaded := Self.DownloadDir + '/' + FImageList[a].[b].AFileName;
            // If Linux or Mac need a / not a \ in the path.
            {$ENDIF}

            inc(c); // counts how many downloads already done so far, for updating the GUI Progressbar
            If FileExists(FLastDownloaded) then
            Form_Main.ImageView.Picture.Jpeg.LoadFromFile(FLastDownloaded);

          end;  // Updates the GUI
          If c > 0 then Form_Main.OnUpdateGUI(round((c/d)*100)) else
                        Form_Main.ProgressBar1.Position := 0;
        end;
      end;

    end;
   end else
  ErrorList.add('Sorry: Could not find or create the download directory: ' + SaveImageDir + ' [' + DateTimeToStr(Now,False) + ']');
  RememberDownloadDir;
  {$IFDEF WINDOWS}
  Try FDownloadedList.SaveToFile(GetCurrentDir + '\OlympusCameraDownloadRecord.txt');
  except
    //beep;
    ErrorList.add('Sorry: Could not save the list of downloaded files at: ' + GetCurrentDir + '\OlympusCameraDownloadRecord.txt' + ' [' + DateTimeToStr(Now,False) + ']');
  end;
  {$ELSE}
  FDownloadedList.SaveToFile(GetCurrentDir + '/OlympusCameraDownloadRecord.txt');
  {$ENDIF}
  FLastDownloadTime := Now;
  IsDownloading := false;
end;

function TOIShareReader.BeautifyDownloadList: TStringList;   // Removes the delimiters $!? for data display
var
  a: integer;
  ts1: string;
begin;
  If FDownloadedList <> nil then
  begin;
    ts1 := FDownloadedList.text;
    ts1 := ts1.Replace('$!?','    ',[rfReplaceAll]);        // NB need to free this TStringlist externally - don't forget
    Result := TStringlist.create;
    Result.text := ts1;
    a := FindDownloadRecord;
    If a > -1 then
    Result[a] := 'RECORD OF DOWNLOADED FILES';             // Removes the DownloadDir string in the TStringlist from user view
    end;                                                   // but maintains that string in the TStringlist so count on delete lines is OK
  end;


end.

