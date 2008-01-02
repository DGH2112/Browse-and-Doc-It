(**

  This module contains a class which handles the checking of updates from the
  Internet for console applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    02 Jan 2008

**)
Unit CheckForUpdates;

Interface

Uses
  SysUtils, MSXML2_TLB, Graphics, Windows;

Type
  (** This class handles the checking of software vesions against the
      internet. **)
  TCheckForUpdates = Class
  Private
    FConHnd : THandle;
    FURL : String;
    FSoftwareID : String;
    FLastUpdateDate : TDateTime;
    FRegRoot : String;
  Protected
    Function CheckForUpdates(strURL : String) : Boolean;
    Function FindPackage(xmlNodeList : IXMLDOMNodeList) : IXMLDOMNode;
    Function GetNamedNodeText(P : IXMLDOMNode; strName : String) : String;
    Function CheckVersionNumber(iMajor, iMinor, iBugFix, iBuild : Integer) : Integer;
    Procedure OutputMsg(strText : String; iColour : TColor = clNone);
    procedure ReadLastUpdateDate;
    procedure WriteLastUpdateDate;
  Public
    Constructor Create(strURL, strSoftwareID, strRegRoot : String;
      boolForceUpdate : Boolean);
    Destructor Destroy; Override;
    Class Procedure Execute(strURL, strSoftwareID, strRegRoot : String;
      boolForceUpdate : Boolean);
  End;

Implementation

Uses
  DGHLibrary, IniFiles {$IFNDEF CONSOLE}, CheckForUpdatesForm {$ENDIF};

ResourceString
  (** A resource string of the start of the checking process. **)
  strCheckingForUpdates = 'Checking for updates...';
  (** A resource string to note the loading of MS XML. **)
  strLoadingMSXML = '  Loading MS XML...';
  (** A resource string to note the lading of the URL **)
  strLoadingURL = '  Loading "%s"...';
  (** A resource string to note the searching for packages. **)
  strGettingPackages = '  Getting packages...';
  (** A resource string to note the package was found. **)
  strFoundPackage = '  Found "%s"...';
  (** A resource string to note the checking of the software version. **)
  strCheckingSoftwareVerNum = '  Checking software version number...';
  (** A resource string to note that the software is up to date. **)
  strYourSoftwareIsUpToDate = '  Your software is up to date!';
  (** A resource string to note that there is an update available. **)
  strThereIsASoftwareUpdate = '  There is a software update available! See the' +
    ' description below for update details...';
  (** A resource string to define that the package was not found. **)
  strPackageNotFound = '  Package "%s" not found!';
  (** A resource string for an exception message. **)
  strExceptionS = '  Exception: %s';
  (** A resource string to prompt to continue. **)
  strPressEnterToContinue = 'Press <Enter> to continue...';
  (** A resource string to prompt that you are using a newer version of software. **)
  strYouAreUsingANewerVersion = '  You are using a newer version of the software than is available from the internet.';

(**

  This is the main interface method for checking the software version.

  @precon  None.
  @postcon Main interface method for checking the software version.

  @param   strURL          as a String
  @param   strSoftwareID   as a String
  @param   strRegRoot      as a String
  @param   boolForceUpdate as a Boolean

**)
Class Procedure TCheckForUpdates.Execute(strURL, strSoftwareID,
  strRegRoot : String; boolForceUpdate : Boolean);

Begin
  TCheckForUpdates.Create(strURL, strSoftwareID, strRegRoot,
    boolForceUpdate).Free;
End;

(**

  This procedure checks the build number of the software against the XML file
  at the given URL to see if there are any updates available.

  @precon  None.
  @postcon Displays messages to the console regarding the versioning of the
           software.

  @param   strURL as a String
  @return  a Boolean

**)
Function TCheckForUpdates.CheckForUpdates(strURL : String) : Boolean;

Var
  xmlDoc : DOMDocument;
  xmlNodeList : IXMLDOMNodeList;
  P : IXMLDOMNode;
  iMajor, iMinor, iBugFix, iBuild : Integer;
  iResult : Integer;

Begin
  Result := False;
  OutputMsg(strLoadingMSXML);
  xmlDoc := CoDOMDocument40.Create;
  Try
    Try
      xmlDoc.ValidateOnParse := True;
      OutputMsg(Format(strLoadingURL, [strURL]));
      xmlDoc.async := False;
      xmlDoc.validateOnParse := True;
      If xmlDoc.load(strURL) And (xmlDoc.parseError.errorCode = 0) Then
        Begin
          OutputMsg(strGettingPackages);
          xmlNodeList := xmlDoc.getElementsByTagName('Package');
          P := FindPackage(xmlNodeList);
          If P <> Nil Then
            Begin
              OutputMsg(Format(strFoundPackage, [FSoftwareID]));
              OutputMsg(strCheckingSoftwareVerNum);
              iMajor := StrToInt(GetNamedNodeText(P, 'Major'));
              iMinor := StrToInt(GetNamedNodeText(P, 'Minor'));
              iBugFix := StrToInt(GetNamedNodeText(P, 'BugFix'));
              iBuild := StrToInt(GetNamedNodeText(P, 'Build'));
              iResult := CheckVersionNumber(iMajor, iMinor, iBugFix, iBuild);
              If iResult = 0 Then
                OutputMsg(strYourSoftwareIsUpToDate, clWhite)
              Else If iResult > 0 Then
                Begin
                  OutputMsg(strYouAreUsingANewerVersion, clRed);
                  OutputMsg('');
                  OutputMsg('  ' +
                    StringReplace(GetNamedNodeText(P, 'Description'), #13#10,
                    #32#32#13#10, [rfReplaceAll]), clLime);
                  {$IFDEF CONSOLE}
                  OutputMsg('');
                  OutputMsg(strPressEnterToContinue);
                  SysUtils.Beep;
                  ReadLn;
                  {$ELSE}
                  SysUtils.Beep;
                  TfrmCheckForUpdates.Stop;
                  {$ENDIF}
                End Else
                Begin
                  OutputMsg(strThereIsASoftwareUpdate, clYellow);
                  OutputMsg('');
                  OutputMsg('  ' +
                    StringReplace(GetNamedNodeText(P, 'Description'), #13#10,
                    #32#32#13#10, [rfReplaceAll]), clLime);
                  {$IFDEF CONSOLE}
                  OutputMsg('');
                  OutputMsg(strPressEnterToContinue);
                  SysUtils.Beep;
                  ReadLn;
                  {$ELSE}
                  SysUtils.Beep;
                  TfrmCheckForUpdates.Stop;
                  {$ENDIF}
                End;
              Result := True;
            End Else
              OutputMsg(Format(strPackageNotFound, [FSoftwareID]), clYellow);
        End Else
          OutputMsg(Format('  %s ("%s")', [xmlDoc.parseError.reason, strURL]), clRed);
    Except
      On E : Exception Do
        OutputMsg(Format(strExceptionS, [E.Message]), clRed);
    End;
    OutputMsg('');
  Finally
    xmlDoc := Nil;
  End;
End;

(**

  This is the constructor method for the TConsoleCheckForupdates class.

  @precon  None.
  @postcon Initialises the class.

  @param   strURL          as a String
  @param   strSoftwareID   as a String
  @param   strRegRoot      as a String
  @param   boolForceUpdate as a Boolean

**)
Constructor TCheckForUpdates.Create(strURL, strSoftwareID,
  strRegRoot : String; boolForceUpdate : Boolean);

Var
  iURLs : Integer;
  iURL : Integer;

Begin
  FURL := strURL;
  FSoftwareID := strSoftwareID;
  FRegRoot := strRegRoot;
  FConHnd := GetStdHandle(STD_OUTPUT_HANDLE);
  ReadLastUpdateDate;
  If boolForceUpdate Or (FLastUpdateDate + 7.0 < Now) Then
    Begin
      OutputMsg(strCheckingForUpdates, clWhite);
      iURLs := CharCount('|', strURL) + 1;
      For iURL := 1 To iURLs Do
        If CheckForUpdates(GetField(FURL, '|', iURL)) Then
          Break;
      WriteLastUpdateDate;
    End;
End;

(**

  This is the destructor method for the TConsoleCheckForupdates class.

  @precon  None.
  @postcon Ensures the Update Form is closed and freed when the class frees.

**)
destructor TCheckForUpdates.Destroy;
begin
  {$IFNDEF CONSOLE}
  TfrmCheckForUpdates.HideUpdates;
  {$ENDIF}
  Inherited Destroy;
end;

(**

  This function searches through the packages looking for the packages with
  the correct ID.

  @precon  xmlNodeList must be a valid list of nodes to search.
  @postcon Searches through the packages looking for the packages with
           the correct ID.

  @param   xmlNodeList as an IXMLDOMNodeList
  @return  an IXMLDOMNode

**)
Function TCheckForUpdates.FindPackage(xmlNodeList : IXMLDOMNodeList) : IXMLDOMNode;

Var
  i, j : Integer;
  Attrib : IXMLDOMNode;

Begin
  Result := Nil;
  For i := 0 To xmlNodeList.length - 1 Do
    For j := 0 To xmlNodeList.Item[i].attributes.length - 1 Do
      Begin
        Attrib := xmlNodeList.item[i].attributes.item[j];
        If Attrib.nodeName = 'ID' Then
          If Attrib.nodeValue = FSoftwareID Then
            Begin
              Result := xmlNodeList.Item[i];
              Break;
            End;
      End;
End;

(**

  This function returns the text associated with the named node.

  @precon  P must be a valid IXMLDOMNode.
  @postcon Returns the text associated with the named node.

  @param   P       as an IXMLDOMNode
  @param   strName as a String
  @return  a String

**)
Function TCheckForUpdates.GetNamedNodeText(P : IXMLDOMNode; strName : String) : String;

Var
  i : Integer;

Begin
  Result := '';
  For i := 0 To P.childNodes.length - 1 Do
    If AnsiCompareText(P.childNodes[i].nodeName, strName) = 0 Then
      Begin
        Result := P.childNodes[i].text;
        Break;
      End;
End;

(**

  This method returns -ve if the software of older than the internet version,
  0 if the same or +ve if it is newer than the internet version.

  @precon  None.
  @postcon Returns -ve if the software of older than the internet version,
           0 if the same or +ve if it is..

  @param   iMajor  as an Integer
  @param   iMinor  as an Integer
  @param   iBugFix as an Integer
  @param   iBuild  as an Integer
  @return  a Integer

**)
Function TCheckForUpdates.CheckVersionNumber(iMajor, iMinor, iBugFix, iBuild : Integer) : Integer;

Var
  iMaj, iMin, iBug, iBui : Integer;

Begin
  GetBuildNumber(iMaj, iMin, iBug, iBui);
  Result := iMaj - iMajor;
  If Result = 0 Then
    Result := iMin - iMinor;
  If Result = 0 Then
    Result := iBug - iBugFix;
  If Result = 0 Then
    Result := iBui - iBuild;
End;

(**

  This procedure outputs a message to the console IF boolOutputMsgs is true.

  @precon  None.
  @postcon Outputs a message to the console IF boolOutputMsgs is true.

  @param   strText as a String
  @param   iColour as a TColor

**)
Procedure TCheckForUpdates.OutputMsg(strText : String; iColour : TColor = clNone);

Begin
  {$IFDEF CONSOLE}
  OutputToConsoleLn(FConHnd, StringReplace(strText, #13#10, #32, [rfReplaceAll]),
    iColour);
  {$ELSE}
  If strText <> '' Then
    TfrmCheckForUpdates.ShowUpdates(StringReplace(strText, #13#10, #32, [rfReplaceAll]),
      iColour);
  {$ENDIF}
End;

(**

  This method reads the last update date from the registry.

  @precon  None.
  @postcon Reads the last update date from the registry.

**)
Procedure TCheckForUpdates.ReadLastUpdateDate;

Var
  iDay, iMonth, iYear : Word;

Begin
  With TIniFile.Create(FRegRoot) Do
    Try
      iDay := ReadInteger('CheckForUpdates', 'Day', 01);
      iMonth := ReadInteger('CheckForUpdates', 'Month', 01);
      iYear := ReadInteger('CheckForUpdates', 'Year', 1900);
      FLastUpdateDate :=  EncodeDate(iYear, iMonth, iDay);
    Finally
      Free;
    End;
End;

(**

  This method writes the last update date to the registry.

  @precon  None.
  @postcon Writes the last update date to the registry.

**)
Procedure TCheckForUpdates.WriteLastUpdateDate;

Var
  iDay, iMonth, iYear : Word;

Begin
  With TIniFile.Create(FRegRoot) Do
    Try
      DecodeDate(Now, iYear, iMonth, iDay);
      WriteInteger('CheckForUpdates', 'Day', iDay);
      WriteInteger('CheckForUpdates', 'Month', iMonth);
      WriteInteger('CheckForUpdates', 'Year', iYear);
    Finally
      Free;
    End;
End;

End.
