unit L_CVSparser;
{CSV file format reader.
 Reads each line and then rereads each line for each delimited string.  Two strings are filled with characters when parsing within
 delimited text. One (SVal) holds a value type string, the other (SStr) holds a string type string.  Always by default, the delimited
 text characters are passed to the value type string.  But if the delimited text contains any quotemarks then that text within the
 quotemarks is passed to the string type string.  Later outside the inner parsing loop, a new fieldsrecord (FieldsData) is created to
 hold the data.  If the SStr string contains characters then it is used to fill the Fieldsdata.SValue value and the FieldsData.FType
 is set to be ftString type.  If no SStr is present then the SVal text is used and the field type is set to ftFloat.

 Note that those values in quotemarks will be considered a text data type and those without quotemarks will be considered a float
 data type.  Quote carefully!

 While string data has been detected during parsing, then the delimiter and end of file and line feed characters are ignored, such
 that text including line ends and paragraphs are considered part of that field's data.  But beware if a single opening QuoteMark
 is present in the file without a closing quotemark then all the rest of the file will be included in that field.  Good to avoid unclosed
 quote marks.

 The characters for end of line (EOL), line feed (LF), the delimiter and the quotemark are all publically available in the class and
 can be set as required.

 21/08/16  Just discovered TStringReader - could have used this in CSVParser to avoid low level char iteration e.g. GetLine would
 be TStringReader.ReadLine.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type
 SField = record
  SValue: String;
  FType:  TFieldType;
 end;

 {**************************}

type TCSVParser = class(TComponent)

 Private

 Public
  FName:      TFileName;
  DataTitle:  String;
  EOL,
  LF,
  Delimiter,
  QuoteMark:  Char;
  FileEnd:    Boolean;
  AllPos,                      // Cursor Position
  LineNo:     integer;         // Line being parsed
  AllText:    String;          // Holds all the file's retrieved text
  FieldsData: array of SField; // Holds a series of data from one line in the file
  Constructor Create(AOwner: TComponent);  override;
  Destructor  Destroy;  override;
  Procedure   LoadCSVStringList(CSVStringList: TStringlist);
  Procedure   LoadCSVFile(CSVFileName: TFilename);
  Procedure   GetFieldsData(LineStr: String);
  Function    GetLine: string;
  Procedure   ResetToStart;
  Procedure   ClearFieldsData;
end;

implementation

{************************************************************************}

Constructor TCSVParser.Create(AOwner: TComponent);
Begin;
 inherited Create(AOwner);
 EOL := #13;      // These will need to be different for Linux or Mac or Android
 LF  := #10;
 Delimiter := ',';
 QuoteMark := '"';
 FileEnd   := false;
 AllPos    := 1;
 LineNo    := 0;
 AllText   := '';
 Setlength(FieldsData,0);
End;

{************************************************************************}

Destructor  TCSVParser.Destroy;
Begin;
 Setlength(FieldsData,0);
 inherited Destroy;
End;

{************************************************************************}

Procedure TCSVParser.LoadCSVStringList(CSVStringList: TStringlist);  // Use this in case you have a TStringlist of the CSV data at run time
begin;
  If (CSVStringList <> nil) then AllText := CSVStringList.Text else
  AllText := '';
end;

{************************************************************************}

Procedure   TCSVParser.LoadCSVFile(CSVFileName: TFilename);
var
 F: text;
 C: char;
Begin;
 If FileExists(CSVFileName) then
 begin;
  FName := CSVFileName;
  AllText := '';
  Try;
   AssignFile(F,FName);
   Reset(F);
   While not Eof(F) do
   begin;
    Read(F,C);
    AllText := AllText + C;
   end;
   CloseFile(F);
  Except;
   CloseFile(F);
  End;
 end;
End;

{************************************************************************}

Procedure  TCSVParser.GetFieldsData(LineStr: String);     // Returns each delimited text phrase into the FieldsData array
var       // TODO:  The end of each line is being polluted with additional non typeable characters which may be part of word processor coding.
          //        but are visible in Delphi strings.    - not sure where these are coming from - needs fixing.
          // TODO:  This problem breaks the parser  - can not load any csv data - some non typed characters present in the strings may be the problem - get this fixed
          // TODO:
 a: integer;
 IsString: boolean;
 SVal, SStr: String;
Begin;
 a := 1;
 ClearFieldsData;
 IsString := false;
 If length(LineStr) > 2 then
 begin;
  Repeat
   SVal := '';
   SStr := '';
   Repeat
    If ((IsString = false) and (LineStr[a] = QuoteMark)) then IsString := true else  // toggles if a string is present
    If ((IsString = true)  and (LineStr[a] = QuoteMark)) then IsString := false;
    If not (LineStr[a] in [EOL, LF, Delimiter, QuoteMark]) then
    begin;
     SVal := SVal + LineStr[a];
     If IsString = true then SStr := SStr + LineStr[a];
    end;
    inc(a);
   If a > length(LineStr) then break;
   Until ((IsString = false) and (LineStr[a] = delimiter)) or  (LineStr[a] = EOL) or (a > length(LineStr)+3);
    Setlength(FieldsData, length(FieldsData) + 1);
    If SStr > '' then
    begin;
     FieldsData[length(FieldsData)-1].SValue := SStr;
     FieldsData[length(FieldsData)-1].FType  := ftString;
    end else
    begin;
     FieldsData[length(FieldsData)-1].SValue := SVal;
     FieldsData[length(FieldsData)-1].FType  :=  ftFloat;  //ftExtended; // in Delphi
    end;
  Until (a > length(LineStr));
 end;
End;

{************************************************************************}

Function TCSVParser.GetLine: string;  // Returns each line of text
 var
  S: String;  // local string buffer
  IsString: Boolean;
Begin;
 Result := '';
 IsString := false;
 setlength(S,3);
 S[1] := chr(32);
 S[2] := chr(32);
 S[3] := chr(32);
 If (length(AllText) > 0) and (AllPos > 0) and (AllPos < length(AllText)) then
 Repeat;
  S[1] := S[2];
  S[2] := S[3];
  S[3] := AllText[AllPos];
  If ((IsString = false) and (S[3] = QuoteMark)) then IsString := true else  // toggles if a string is present
  If ((IsString = true)  and (S[3] = QuoteMark)) then IsString := false;
  Result := Result + S[1];
  Inc(AllPos);
 Until ( (IsString = false) and
       ( (S[2] = EOL)       and
         (S[3] = LF)  )     or
         (S = '#$D')
       )
 or  // exits when gets to TStringlist line end code
 (AllPos >= length(AllText) + 2 );   // +1 because S1 is now the char two before AllPos
 Inc(LineNo);
  If length(result) > 1 then Delete(Result,1,2);   // Added this line 07/01/20 as initial S2,S3 which becomes S1,S2 are spaces #32 and not in the original file
 if AllPos >= length(AllText) then FileEnd := true;
End;

{************************************************************************}

Procedure TCSVParser.ResetToStart;
Begin;
 AllPos    := 1;
 LineNo    := 0;
 Setlength(FieldsData,0);
 FileEnd   := false;
End;

{************************************************************************}

Procedure TCSVParser.ClearFieldsData;
begin;
 Setlength(FieldsData,0);
end;

{************************************************************************}

end.


