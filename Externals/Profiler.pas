(**

  This module contains a class to monitor profiling information in a tree of
  profiles.

  @Version 1.0
  @Date    20 Feb 2013
  @Author  David Hoyle

**)
Unit Profiler;

Interface

Uses
  Classes,
  Contnrs {$IFNDEF CONSOLE},
  Forms,
  StdCtrls {$ENDIF};

{$IFDEF PROFILECODE}
Type
  (** This class represent a single element of profile information. It can
      contain sub elements of itself to build up and stack similar to that
      of the code being profiled. **)
  TProfile = Class
  Strict Private
    FMethodName   : String;
    FStartTick    : Double;
    FDurationTick : Double;
    FInProcessTick: Double;
    FCallCount    : Int64;
    FProfiles     : TObjectList;
    FParent       : TProfile;
    FStackDepth   : Int64;
  Strict Protected
    Function FindProfile(strMethodName: String; iStackDepth: Integer): TProfile;
    Procedure StartTiming;
    (**
      This property returns the total duration of the profiles calls, i.e. TickCount.
      @precon  None.
      @postcon Returns the total duration of the profiles calls.
      @return  a Double
    **)
    Property DurationTick: Double Read FDurationTick;
  Public
    Constructor Create(strMethod: String; iStackDepth: Integer; objParent: TProfile);
    Destructor Destroy; Override;
    Function StartProfile(strMethodName: String; iStackDepth: Integer): TProfile;
    Function StopProfile: TProfile;
    Procedure DumpProfileInformation(slProfileFile: TStringList);
    (**
      This property returns the number of Call Counts on the profile.
      @precon  None.
      @postcon Returns the number of Call Counts on the profile.
      @return  an Int64
    **)
    Property CallCount: Int64 Read FCallCount;
  End;

  (** This class handles all the TProfile instances in a tree structure. **)
  TProfiler = Class
  Strict Private
    FStackTop      : Integer;
    FRootProfile   : TProfile;
    FCurrentProfile: TProfile;
    {$IFNDEF CONSOLE}
    FProgressForm  : TForm;
    FLabel         : TLabel; 
    {$ENDIF}
  Strict Protected
    Procedure DumpProfileInformation;
    {$IFNDEF CONSOLE}
    Procedure Msg(strMsg: String);
    {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Start(strMethodName: String);
    Procedure Stop;
  End;

Var
  (** This is a public variable which give the calling code access to the
      instance of the TPRofiler class. **)
  CodeProfiler: TProfiler;
{$ENDIF}

Implementation

Uses
  SysUtils,
  Windows {$IFNDEF CONSOLE},
  Controls {$ENDIF};

{$IFDEF PROFILECODE}
(**

  This method returns the performance tick count from the system.

  @precon  None.
  @postcon Returns the performance tick count from the system.

  @return  an Double

**)
Function TickTime: Double;

Var
  t, f: Int64;
Begin
  QueryPerformanceCounter(t);
  QueryPerformanceFrequency(f);
  Result := 1000.0 * Int(t) / Int(f);
End;

(**


  This is a constructor for the TProfile class.


  @precon  None.

  @postcon Creates a container for the sub profiles.


  @param   strMethod   as a String
  @param   iStackDepth as an Integer
  @param   objParent   as a TProfile

**)
Constructor TProfile.Create(strMethod: String; iStackDepth: Integer;
  objParent: TProfile);

Begin
  FProfiles      := TObjectList.Create(True);
  FDurationTick  := 0;
  FInProcessTick := 0;
  FMethodName    := strMethod;
  FParent        := objParent;
  FStackDepth    := iStackDepth;
End;

(**


  This is a destructor for the TProfile class.

  @precon  None.
  @postcon Frees sub profiles.


**)
Destructor TProfile.Destroy;
Begin
  FProfiles.Free;
  Inherited Destroy;
End;

(**


  This method outputs the profiles information to the given file handle.


  @precon  None.

  @postcon Outputs the profiles information to the given file handle.


  @param   slProfileFile as a TStringList

**)
Procedure TProfile.DumpProfileInformation(slProfileFile: TStringList);

Var
  i   : Integer;
  P   : TProfile;
  iPos: Integer;

Begin
  If FMethodName <> '' Then
    Begin
      For i := 0 To FProfiles.Count - 1 Do
        Begin
          P              := FProfiles[i] As TProfile;
          FInProcessTick := FInProcessTick - P.DurationTick;
        End;
      iPos := Pos('.', FMethodName);
      slProfileFile.Add(Format('%d,%s,%s,%1.4f,%1.4f,%d', [
        FStackDepth,
        Copy(FMethodName, 1, iPos - 1),
        Copy(FMethodName, iPos + 1, Length(FMethodName) - iPos),
        FDurationTick,
        FInProcessTick,
        FCallCount
        ]));
    End;
  For i := 0 To FProfiles.Count - 1 Do
    (FProfiles[i] As TProfile).DumpProfileInformation(slProfileFile);
End;

(**

  This method attempts to find the named method in the profile collection. If
  found the profile is returned else a new profile is added and that profile
  returned.

  @precon  None.
  @postcon Attempts to find the named method in the profile collection. If
           found the profile is returned else a new profile is added and that
           profile returned.

  @param   strMethodName as a String
  @param   iStackDepth   as an Integer
  @return  a TProfile

**)
Function TProfile.FindProfile(strMethodName: String; iStackDepth: Integer): TProfile;

Var
  i: Integer;
  P: TProfile;

Begin
  For i := 0 To FProfiles.Count - 1 Do
    Begin
      P := FProfiles[i] As TProfile;
      If AnsiCompareText(P.FMethodName, strMethodName) = 0 Then
        Begin
          Result := FProfiles[i] As TProfile;
          Exit;
        End;
    End;
  Result := TProfile.Create(strMethodName, iStackDepth, Self);
  FProfiles.Add(Result);
End;

(**

  This method starts the process of monitoring the current methods profile
  session.

  @precon  None.
  @postcon Starts the process of monitoring the current methods profile
           session.

  @param   strMethodName as a String
  @param   iStackDepth   as an Integer
  @return  a TProfile

**)
Function TProfile.StartProfile(strMethodName: String; iStackDepth: Integer): TProfile;

Begin
  Result := FindProfile(strMethodName, iStackDepth);
  Result.StartTiming;
End;

(**

  This method starts the timing of the current profile session.

  @precon  None.
  @postcon Starts the timing of the current profile session.

**)
Procedure TProfile.StartTiming;

Begin
  FStartTick := TickTime;
  Inc(FCallCount);
End;

(**

  This method stop profiling the current method and returns the parent profile.

  @precon  None.
  @postcon Stop profiling the current method and returns the parent profile.

  @return  a TProfile

**)
Function TProfile.StopProfile: TProfile;

Begin
  FDurationTick  := FDurationTick + (TickTime - FStartTick);
  FInProcessTick := FDurationTick; { Make the same as FDuration. A call to
                                     calculate this needs to be made after
                                     profiling and before dumping the
                                     information. }
  Result := FParent;
End;

{ TProfiler }

(**


  This is a constructor for the TProfiler class.

  @precon  None.
  @postcon Creates an initial TProfile instance for the tree structure.


**)
Constructor TProfiler.Create;

Begin
  {$IFNDEF CONSOLE}
  FProgressForm               := TForm.Create(Nil);
  FProgressForm.BorderStyle   := bsToolWindow;
  FProgressForm.Caption       := 'Profiling...';
  FProgressForm.ClientWidth   := 400;
  FProgressForm.ClientHeight  := 50;
  FProgressForm.Top           := Screen.Height - FProgressForm.Height;
  FProgressForm.Left          := Screen.Width - FProgressForm.Width;
  FProgressForm.FormStyle     := fsStayOnTop;
  FProgressForm.Margins.Left  := 5;
  FProgressForm.Margins.Right := 5;
  FLabel                     := TLabel.Create(FProgressForm);
  FLabel.Parent              := FProgressForm;
  FLabel.Align               := alClient;
  FLabel.Layout              := tlCenter;
  FLabel.Caption             := 'Loading...';
  FLabel.Font.Name           := 'Arial';
  FLabel.Font.Size           := 9;
  FLabel.WordWrap            := True;
  FLabel.EllipsisPosition    := epEndEllipsis;
  FProgressForm.Show;
  Msg('Profiler started!');
  {$ENDIF}
  FRootProfile := TProfile.Create('', 0, Nil);
End;

(**


  This is a destructor for the TProfiler class.

  @precon  None.
  @postcon Frees the memory used by the profile tree.


**)
Destructor TProfiler.Destroy;
Begin
  DumpProfileInformation;
  FRootProfile.Free;
  {$IFNDEF CONSOLE}
  FProgressForm.Free;
  {$ENDIF}
  Inherited Destroy;
End;

(**


  This method starts the process of calculating and outputting the profile tree.

  @precon  None.
  @postcon Starts the process of calculating and outputting the profile tree.

**)
Procedure TProfiler.DumpProfileInformation;

Var
  strBuffer                     : Array [0 .. MAX_PATH] Of Char;
  strModuleFileName, strFileName: String;
  slProfile                     : TStringList;

Begin
  {$IFDEF CONSOLE}
  WriteLn('Processing the profile information...');
  {$ELSE}
  Msg('Processing the profile information...');
  {$ENDIF}
  GetModuleFileName(hInstance, strBuffer, MAX_PATH);
  strFileName       := StrPas(strBuffer);
  strModuleFileName := strFileName;
  strFileName       := ChangeFileExt(strFileName, '.profile');
  slProfile         := TStringList.Create;
  Try
    {$IFNDEF CONSOLE}
    Msg('Loading existing data...');
    {$ELSE}
    WriteLn('Loading existing data...');
    {$ENDIF}
    If FileExists(strFileName) Then
      slProfile.LoadFromFile(strFileName);
    slProfile.Add('Profile Dump For Application ' + strModuleFileName + ' on ' +
      FormatDateTime('ddd dd/mmm/yyyy @ hh:mm:ss', Now));
    slProfile.Add(Format('%s,%s,%s,%s,%s,%s', [
      'Stack Depth',
      'Class',
      'Method Name',
      'Total Tick Count (ms)',
      'In Process Tick Count (ms)',
      'Call Count'
      ]));
    {$IFNDEF CONSOLE}
    Msg('Processing new data...');
    {$ELSE}
    WriteLn('Loading existing data...');
    {$ENDIF}
    FRootProfile.DumpProfileInformation(slProfile);
    {$IFNDEF CONSOLE}
    Msg('Saving data...');
    {$ELSE}
    WriteLn('Saving data...');
    {$ENDIF}
    slProfile.SaveToFile(strFileName);
  Finally
    slProfile.Free;
  End;
  Sleep(250);
End;

{$IFNDEF CONSOLE}
(**

  This method outputs a message to the profile form.

  @precon  None.
  @postcon Outputs a message to the profile form.

  @param   strMsg as a String

**)
Procedure TProfiler.Msg(strMsg: String);

Begin
  FLabel.Caption := strMsg;
  Application.ProcessMessages;
End;
{$ENDIF}

(**


  This method stops the profiling of the current method.

  @precon  None.
  @postcon Stops the profiling of the current method.


**)
Procedure TProfiler.Stop;

Begin
  FStackTop := FStackTop - 1;
  If FStackTop < 0 Then
    FStackTop     := 0;
  FCurrentProfile := FCurrentProfile.StopProfile;
  If FStackTop <= 0 Then
    Msg('Idle.');
End;

(**


  This method starts the profiling of the current method.

  @precon  None.
  @postcon Starts the profiling of the current method.


  @param   strMethodName as a String

**)
Procedure TProfiler.Start(strMethodName: String);

Begin
  If FStackTop = 0 Then
    FCurrentProfile := FRootProfile;
  FStackTop         := FStackTop + 1;
  FCurrentProfile   := FCurrentProfile.StartProfile(strMethodName, FStackTop);
  Msg('Profiling: ' + strMethodName);
End;
{$ENDIF}

(** Creates the profiler on loading the module. **)
Initialization
  {$IFDEF PROFILECODE}
  CodeProfiler := TProfiler.Create;
  {$ENDIF}
(** Frees (and writes data) the profiler on unloading the module **)
Finalization
  {$IFDEF PROFILECODE}
  CodeProfiler.Free;
  {$ENDIF}
End.
