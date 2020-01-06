unit Olympus_ImageSave1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  ExtCtrls, ComCtrls, ShellCtrls, Arrow, EditBtn, Spin, Buttons, RTTICtrls,
  OlympusShare, fphttpclient;
type

  { TForm_Main }

  TForm_Main = class(TForm)
    Arrow_Transfer: TArrow;
    Btn_Get: TButton;
    Btn_MarkAllDLed: TButton;
    Btn_RefreshSD: TButton;
    Btn_Refresh1: TButton;
    Btn_DeleteDLHxFiles: TButton;
    Btn_CancelDL: TButton;
    CBox_DLHx: TCheckBox;
    ChBox_Timer: TCheckBox;
    CB_SDFiles: TCheckBox;
    ChLBox_DLHx: TCheckListBox;
    CLBox_SDImages: TCheckListBox;
    CLBox_SDDir: TCheckListBox;
    DirEdit_Images: TDirectoryEdit;
    Edit_ServerAddr: TEdit;
    Image_Banner: TImage;
    ImageView: TTIImage;
    Label1: TLabel;
    Label5: TLabel;
    Label_CB_DLHx: TLabel;
    Label_Transfer: TLabel;
    Label11: TLabel;
    Label_TimeSet: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label_UseTimeSet: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label_UseTimeSet1: TLabel;
    Label_UseTimeSet2: TLabel;
    Memo1: TMemo;
    Memo_Errors: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel_LastImage: TPanel;
    Panel_DownloadHxTitle: TPanel;
    Panel_DownloadHxTitle1: TPanel;
    ProgressBar1: TProgressBar;
    ShListView_Files: TShellListView;
    TS_Transfer: TTabSheet;
    TS_Settings: TTabSheet;
    TSDownloadHx: TTabSheet;
    Timer_Transfer: TTimer;
    UpDown_TimeSet: TUpDown;

    //procedure Button1Click(Sender: TObject);  // original proof of concept
    procedure Btn_CancelDLClick(Sender: TObject);
    procedure Btn_MarkAllDLedClick(Sender: TObject);
    procedure Btn_Refresh1Click(Sender: TObject);
    procedure CLBox_SDDirClick(Sender: TObject);
    Procedure UpdateDLoadList;
    Procedure OnUpdateGUI(PercentDone: integer);
    procedure Arrow_TransferClick(Sender: TObject);
    Procedure StartTransfer;
    procedure Btn_DeleteDLHxFilesClick(Sender: TObject);
    Procedure DisplaySDCardFiles(SelDir: integer);
    procedure Btn_RefreshSDClick(Sender: TObject);
    procedure CBox_DLHxClick(Sender: TObject);
    procedure ChBox_TimerClick(Sender: TObject);
    procedure CB_SDFilesClick(Sender: TObject);
    procedure ChLBox_DLHxSelectionChange(Sender: TObject; User: boolean);
    procedure DirEdit_ImagesChange(Sender: TObject);
    procedure Edit_ServerAddrExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer_TransferTimer(Sender: TObject);
    procedure UpDown_TimeSetChanging(Sender: TObject; var AllowChange: Boolean);
  private

  public

  end;

var
  Form_Main: TForm_Main;
  OiLink:  TOiShareReader;

implementation

{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  OiLink := TOiShareReader.create(Form_Main);
  Edit_ServerAddr.text     := 'http://oishare';
  OiLink.ServerAddr        := Edit_ServerAddr.text;
  DirEdit_Images.Text      := OiLink.DownloadDir;
  DirEdit_Images.Directory := DirEdit_Images.Text;
  ShListView_Files.Root    := DirEdit_Images.Text;
  UpdateDLoadList;

  Timer_Transfer.interval := UpDown_TimeSet.Position;
  Timer_Transfer.Enabled := ChBox_Timer.Checked;
  If ChBox_Timer.Checked then
  begin;
    Label_Transfer.caption  := 'Transfering Timer On';
    Timer_Transfer.enabled := true;
  end else
  begin;
    Label_Transfer.caption  := 'Transfer Files Now';
    Timer_Transfer.enabled := false;
  end;
end;

Procedure TForm_Main.UpdateDLoadList;
begin
  ChLBox_DLHx.Items.Text := OiLink.BeautifyDownloadList.text;
  ChLBox_DLHx.CheckAll(cbChecked);
end;

Procedure TForm_Main.OnUpdateGUI(PercentDone: integer);
begin;
  Progressbar1.position := PercentDone;
  Application.Processmessages;
end;

procedure TForm_Main.Btn_Refresh1Click(Sender: TObject);
begin
   ShListView_Files.Root := '';
   ShListView_Files.Root := OiLink.DownloadDir;
end;

procedure TForm_Main.Btn_CancelDLClick(Sender: TObject);
begin
  OiLink.IsDownloading := false;
end;

procedure TForm_Main.Btn_MarkAllDLedClick(Sender: TObject);
begin
If MessageDlg('Confirm Mark All SD Card Files As Downlowded?',
              'Are you sure you want to mark all current SD card files as already downlowded?',
               mtConfirmation ,[mbYes, mbNo],0) = mrYes then
 OiLink.RegisterAllSDCardFilesAsDownloaded;
 Self.DisplaySDCardFiles(0);
 UpdateDLoadList;
end;

procedure TForm_Main.CLBox_SDDirClick(Sender: TObject); // Identifies which SD card directory is selected in the CheckListBox
var
  a,b: integer;
begin
  a := 0;  b := -1;
  If CLBox_SDDir.Count > 0 then
  Repeat
    If CLBox_SDDir.Selected[a] then b := a;
    inc(a);
  until b > -1;
  DisplaySDCardFiles(b);
end;


procedure TForm_Main.Timer_TransferTimer(Sender: TObject);
begin
  StartTransfer;
end;

procedure TForm_Main.UpDown_TimeSetChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  Label_TimeSet.caption := InttoStr(UpDown_TimeSet.Position);
  Timer_Transfer.Interval := 1000 * UpDown_TimeSet.Position;
end;

procedure TForm_Main.ChBox_TimerClick(Sender: TObject);
begin  {Checking the repeat timer checkbox sets the interval and starts the timer. If the timer was previously active and the repeat timer
        checkbox  is clicked off then the timer is disabled but any current timed download is allowed to continue until finished and of course will
        not reoccur anymore}
  Timer_Transfer.Interval := 1000 * UpDown_TimeSet.Position; // always update the timer interval
  If ChBox_Timer.checked then
  begin; // click on
    If not Timer_Transfer.enabled then
    begin;
    Label_Transfer.caption := 'Transfer Timer On';
    Timer_Transfer.enabled := true;
    end;
  end else
  begin; // click off
    If Timer_Transfer.enabled = true then
    begin;                                             // switch off the timer
      Timer_Transfer.enabled := false;                 // but let current timed download complete
      If Oilink.Isdownloading = false then             // but if no active download happening then
      Label_Transfer.caption := 'Transfer Files Now';  // reset the caption
    end;
  end;
end;

procedure TForm_Main.CBox_DLHxClick(Sender: TObject);  // checks or unchecks all records of downloaded files
begin
  If CBox_DLHx.checked then
  ChLBox_DLHx.CheckAll(cbChecked) else
  ChLBox_DLHx.CheckAll(cbUnChecked);
end;

procedure TForm_Main.CB_SDFilesClick(Sender: TObject);     // Checks or unchecks all SD card files listed
begin
  If CB_SDFiles.checked then
  CLBox_SDImages.CheckAll(cbChecked) else
  CLBox_SDImages.CheckAll(cbUnChecked);
end;

procedure TForm_Main.ChLBox_DLHxSelectionChange(Sender: TObject; User: boolean);
var                                                        // Sets the checkbox to checked if files are also already selected
  a: integer;
begin
  For a := 0 to ChLBox_DLHx.Count -1 do
  begin;
    If ChLBox_DLHx.Selected[a]      then
    ChLBox_DLHx.Checked[a] := true  else
    ChLBox_DLHx.Checked[a] := false;
    CBox_DLHx.checked := false;
  end;
end;

procedure TForm_Main.Btn_DeleteDLHxFilesClick(Sender: TObject); // Deletes records of previously downloaded files from DownloadList
var                                                             // This means that if the file is found again on the SD card then it will be downloaded again
  a, DLDir: integer;
begin
 a := 0;
 Repeat
   If (ChLBox_DLHx.Checked[a]) then
   begin
     If a > 0 then // so dont delete DownloadDir record, expected to be string [0]
     begin;
       ChLBox_DLHx.items.Delete(a);
       OiLink.DownloadList.Delete(a);
       If a > 0 then dec(a);
     end;
   end;
   inc(a);
 Until a > ChLBox_DLHx.count -1;
 OiLink.CountFilesForDownload;  // updates the FImageList arrays as well as counts
end;

Procedure TForm_Main.StartTransfer; // Sets up the GUI for file transfer and then calls the server and retrieves the files
begin;
  If (OiLink <> nil) and
     (OiLink.IsDownloading = false) then
   begin;
     If OiLink.IsDownloading then exit;
     ProgressBar1.position := 0;
     Arrow_Transfer.color    := clGreen;
     Screen.Cursor           := crHourglass;
     Label_Transfer.caption  := 'Transfering Files Now...';
     OiLink.ServerAddr       := Edit_ServerAddr.text; //http://oishare
     OiLink.DownloadDir      := DirEdit_Images.Directory;
     OiLink.GetSDCardData;  // update SDcard directory and files listing
     DisplaySDCardFiles(length(OiLink.ImageLists)-1); // show the first directory and its files
     Btn_CancelDL.visible     := true;
     Application.ProcessMessages;

     OiLink.DownloadImages(DirEdit_Images.Directory);

     Btn_CancelDL.visible     := false;
     If Timer_Transfer.enabled then
     Label_Transfer.caption  := 'Transfer Timer On' else
     Label_Transfer.caption  := 'Transfer Files Now';
     Memo_Errors.lines.text  := OiLink.ErrorList.text;
     ChLBox_DLHx.items.Text  := OiLink.BeautifyDownloadList.text;
     ChLBox_DLHx.CheckAll(cbChecked);
     Arrow_Transfer.color    := $00606060;
     Screen.Cursor           := crDefault;
     If Oilink.LastDownlowdedImage <> '' then
     ImageView.Picture.Jpeg.LoadFromFile(Oilink.LastDownlowdedImage);
     ProgressBar1.position := 0;
     DisplaySDCardFiles(length(OiLink.ImageLists)-1); // updates display of which files have now been downloaded
     ShListView_Files.Root := '';
     ShListView_Files.Root := OiLink.DownloadDir;
     Application.ProcessMessages;

   end;
end;

procedure TForm_Main.Arrow_TransferClick(Sender: TObject);
begin
  StartTransfer;
end;

Procedure TForm_Main.DisplaySDCardFiles(SelDir: integer);  // Does what it says on the tin!
var
  SL:TStringlist;  // use a transient Stringlist so that the memory can be freed on each time used to avoid a memory leak
begin;
 If (length(OiLink.DirList) > 0) then
 begin;
   SL := OiLink.DCIMListToStringList(OiLink.DirList,[AFilename]);
   CLBox_SDDir.Items.text := SL.text;
   SL.Free;
   If (length(OiLink.ImageLists) > 0) then
   begin;
     If SelDir < 0 then SelDir    := 0;
     If length(OiLink.ImageLists) = 0 then
     SelDir := length(OiLink.ImageLists)-1;
     CLBox_SDDir.Selected[SelDir] := true;
     CLBox_SDDir.Checked[SelDir]  := true;
     SL := TStringlist.create;
     SL := OiLink.DCIMListToStringList(OiLink.ImageLists[SelDir],[AFilename, ADownloaded]);
     CLBox_SDImages.Items.text := SL.text;
     SL.Free;
   end else
   CLBox_SDImages.Items.text := 'No Data';
 end;
end;

procedure TForm_Main.Btn_RefreshSDClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  Oilink.GetSDCardData;
  DisplaySDCardFiles(length(OiLink.ImageLists)-1);
  Screen.Cursor := crDefault;
end;

procedure TForm_Main.DirEdit_ImagesChange(Sender: TObject);
begin
 ShListView_Files.root := DirEdit_Images.Directory;
 ShListView_Files.Refresh;
 OiLink.DownloadDir:= DirEdit_Images.Directory;
 OiLink.RememberDownloadDir;
end;

procedure TForm_Main.Edit_ServerAddrExit(Sender: TObject);
begin
    OiLink.ServerAddr := Edit_ServerAddr.text;
end;

procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 {$IFDEF WINDOWS}
 //OiLink.ErrorList.SaveToFile(GetCurrentDir + '\' + 'OlympusCameraLinkErrorLog[' + FormatDateTime('dd_mm_yyyy_hh_nn_ss',Now) + '].txt' );
 {$ELSE}
 OiLink.ErrorList.SaveToFile(GetCurrentDir + '/' + 'OlympusCameraLinkErrorLog[' + FormatDateTime('dd_mm_yyyy_hh_nn_ss',Now) + '].txt' );
 {$ENDIF}
 OiLink.free;

end;


{  This is the original proof of concept code without object orientation. Not used now

procedure TForm_Main.Button1Click(Sender: TObject);
{var
   HTTPRequest: String;
   ts1: string;
   S1, S2: integer;
   HTTPResponse,
   DList,
   FList: TStringlist;
   FStream: TMemoryStream;
   FName: TFilename;
   a: integer;
   APath, AFilename, AFileSize, AUnknown, ADate, ATime: String;

begin



{// init
  FName        := 'C:\MG\IMAGEBANK\DENTAL\Today\TodayPic\Testimage1.jpg';
  HTTPResponse := TStringlist.create;
  DList        := TStringlist.create;
  FList        := TStringlist.create;
  FStream      := TMemoryStream.create;
  HTTPRequest  := 'http://oishare/DCIM/101OLYMP/*.*';

  // Try to access SD card via wireless LAN to retrive the directories and filelists from the Olympus camera's oishare server
  HTTPResponse.Clear;
  Try


   HTTPClient1.SimpleGet(HTTPRequest, HTTPResponse);  // removed while debugging
  except;
    Showmessage('Sorry: the Olympus camera could not be reached at ' + HTTPRequest);
    exit;
  end;
  // for debugging and testing
  //HTTPResponse.LoadFromFile('C:\MG\MGSoftware\Lazarus\OlympusImageSave\olympus_camera_oishare_httpresponse.html');
  If (HTTPResponse.Count > 0) and
     (HTTPResponse.Text.Contains('wlansd = new Array();')) then  // ie. The httpresponse contains a javascript array object called wlansd

  For a := 0 to HTTPResponse.count -1 do
  begin;
    If (copy(HTTPResponse[a],1,7) = 'wlansd[') and
       (HTTPResponse[a].Contains('=')) then // The Javascript wlansd object elements lines each begins with 'wlansd[' and has an '=' in it
    begin;  // i.e. this line is a Javascript wlansd object array element representing one image file
      ts1 := HTTPResponse[a];
      S1 := pos('=', ts1) + 2;     // locates the second character after the '=' sign in each wlansd object array line
      If (S1 > 0) and (S1 <= length(ts1)) then
      begin;
        ts1 := copy(ts1, S1, length(ts1) - S1 -1); // grabs the quoted string including six comma separated data values, also not including the trailing ;
        // remove the two " quotation characters
        If pos('"', ts1) > 0 then delete(ts1, pos('"', ts1), 1);
        If pos('"', ts1) > 0 then delete(ts1, pos('"', ts1), 1);
        // Now parse for the six comma separated data in this array

        // APath
        S2 := Pos(',',ts1) ;
        If S2 > 0 then
        begin;
          APath := copy(ts1, 1, S2 -1); // -1 so removes the , at the end of the http get path call
          Delete(ts1,1,S2);             // NB: The path name does not include the server name 'http://oishare',
                                        // and uses unix '/' path delimiters.  The path includes an initial and trailing '/'
          If pos('/*.*', APath) > 0 then delete(APath, pos('/*.*', APath), 4) else // Gets rid of /*.*
          If APath[length(APath)] = '/' then delete(APath, length(APath), 1)   ;   // or gets rid of / if present on path string
          // i.e the path has no trailing forward slash or *.* any more. But actually this works on requesting the file list from the server.
        end;
        //AFilename
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          AFileName := copy(ts1, 1, S2-1);
          Delete(ts1,1,S2);
        end;
        //AFileSize
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          AFileSize := copy(ts1, 1, S2-1);
          Delete(ts1,1,S2);
        end;
        //AUnknown
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          AUnknown := copy(ts1, 1, S2-1);
          Delete(ts1,1,S2);
        end;
        //ADate
        S2 := Pos(',',ts1);
        If S2 > 0 then
        begin;
          ADate := copy(ts1, 1, S2-1);
          Delete(ts1,1,S2);
        end;
        //ATime
        S2 := Pos(',',ts1);
        If (S2 = 0) and (length(ts1) > 0) then    // ie no commas left
        begin;
          ATime := copy(ts1, 1, length(ts1));
          //Delete(ts1,1,S2);
        end;
        //If a < 21 then
        //Showmessage(APath);
        // Now we have the path and filename for each file we can try to call the server to download it

        HTTPRequest := 'http://oishare' + APath + AFileName;
        Try
           //HTTPClient1.SimpleGet(HTTPRequest,FStream);
           //FStream.Position:= 0;
           //FStream.SaveToFile('C:\MG\IMAGEBANK\DENTAL\Today\TodayPic\' + AFileName);
        except
          Showmessage('Sorry: file ' + AFileName + 'could not be saved to PC with ' + HTTPRequest);
        end;


       //Obviously need to run this by putting camera onto wifi transmit and have computer automatically connect to this signal, then will have to manually
       //switch the camera off when done.

       //Would be ideal to have a GUI indiating image transfer and showing images as they come through.

       //Can run this on the Jetson Nano.

      end;
    end;
  end;

  FStream.free;
  FList.free;

end;
}


end.

