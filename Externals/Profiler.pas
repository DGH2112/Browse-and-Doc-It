(**

  This module contains a class to monitor profiling information in a tree of
  profiles.

  @Version 1.0
  @Date    23 Sep 2008
  @Author  David Hoyle

**)
Unit Profiler;

Interface

Uses
  Classes, Contnrs;

Type
  (** This class represent a single element of profile information. It can
      contain sub elements of itself to build up and stack similar to that
      of the code being profiled. **)
  TProfile = Class
  Strict Private
    FMethodName    : String;
    FStartTick     : Int64;
    FDurationTick  : Int64;
    FInProcessTick : Int64;
    FCallCount     : Int64;
    FProfiles      : TObjectList;
    FParent        : TProfile;
    FStackDepth    : Int64;
  Strict
  private
    Function FindProfile(strMethodName : String; iStackDepth : Integer) : TProfile;
    Procedure StartTiming;
    Function GetTotalTime: Int64;
    (**
      This property returns the total duration of the profiles calls, i.e. TickCount.
      @precon  None.
      @postcon Returns the total duration of the profiles calls.
      @return  a Int64
    **)
    Property DurationTick : Int64 Read FDurationTick;
    (**
      This property returns the total duration of all the profiles sub profile calls.
      @precon  None.
      @postcon Returns the total duration of all the profiles sub profile calls
      @return  a Int64
    **)
    Property TotalTime : Int64 Read GetTotalTime;
  published
  Public
    Constructor Create(strMethod : String; iStackDepth : Integer; objParent : TProfile);
    Destructor Destroy; Override;
    Function StartProfile(strMethodName : String; iStackDepth : Integer) : TProfile;
    Function StopProfile : TProfile;
    Procedure DumpProfileInformation(slProfileFile : TStringList);
  End;

  (** This class handles all the TProfile instances in a tree structure. **)
  TProfiler = Class
  Strict Private
    FStackTop       : Integer;
    FRootProfile    : TProfile;
    FCurrentProfile : TProfile;
  Strict Protected
    Procedure DumpProfileInformation;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Start(strMethodName : String);
    Procedure Stop;
  End;

Var
  CodeProfiler : TProfiler;

Implementation

Uses
  SysUtils, Windows;

(**

  This is a constructor for the TProfile class.

  @precon  None.
  @postcon Creates a container for the sub profiles.

  @param   strMethod   as a String
  @param   iStackDepth as an Integer
  @param   objParent   as a TProfile

**)
constructor TProfile.Create(strMethod : String; iStackDepth : Integer;
  objParent : TProfile);

begin
  FProfiles      := TObjectList.Create(True);
  FDurationTick  := 0;
  FInProcessTick := 0;
  FMethodName    := strMethod;
  FParent        := objParent;
  FStackDepth    := iStackDepth;
end;

(**

  This is a destructor for the TProfile class.

  @precon  None.
  @postcon Frees sub profiles.

**)
destructor TProfile.Destroy;
begin
  FProfiles.Free;
  Inherited Destroy;
end;

(**

  This method outputs the profiles information to the given file handle.

  @precon  None.
  @postcon Outputs the profiles information to the given file handle.

  @param   slProfileFile as a TStringList

**)
Procedure TProfile.DumpProfileInformation(slProfileFile : TStringList);

Var
  i : Integer;
  P : TProfile;
  iPos: Integer;

Begin
  If FMethodName <> '' Then
    Begin
      For i := 0 To FProfiles.Count - 1 Do
        Begin
          P := FProfiles[i] As TProfile;
          FInProcessTick := FInProcessTick - P.DurationTick;
        End;
      iPos := Pos('.', FMethodName);
      slProfileFile.Add(Format('%d,%s,%s,%d,%d,%d,%1.1f,%1.1f', [
        FStackDepth,
        Copy(FMethodName, 1, iPos - 1),
        Copy(FMethodName, iPos + 1, Length(FMethodName) - iPos),
        FDurationTick,
        FInProcessTick,
        FCallCount,
        Int(FDurationTick) / Int(FCallCount),
        Int(FInProcessTick) / Int(FCallCount)
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

  @param   strMethod as a String
  @param   iStackDepth as a Integer
  @return  a TProfile

**)
Function TProfile.FindProfile(strMethodName : String; iStackDepth : Integer) : TProfile;

Var
  i : Integer;
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

  This method is a getter method for the TotalTime property.

  @precon  None.
  @postcon Returns the total time for all sub profiles.

  @return  an Int64

**)
function TProfile.GetTotalTime: Int64;

Var
  i : Integer;

Begin
  Result := 0;
  For i := 0 To FProfiles.Count - 1 Do
    Inc(Result, (FProfiles[i] As TProfile).DurationTick);
End;

(**

  This method starts the process of monitoring the current methods profile session.

  @precon  None.
  @postcon Starts the process of monitoring the current methods profile session.

  @param   strMethod as a String
  @param   iStackDepth as a Integer
  @return  a TProfile

**)
Function TProfile.StartProfile(strMethodName : String; iStackDepth : Integer) : TProfile;

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
  FStartTick := GetTickCount;
  Inc(FCallCount);
End;

(**

  This method stop profiling the current method and returns the parent profile.

  @precon  None.
  @postcon Stop profiling the current method and returns the parent profile.

  @return  a TProfile

**)
Function TProfile.StopProfile : TProfile;

Begin
  FDurationTick  := FDurationTick + (GetTickCount - FStartTick);
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
constructor TProfiler.Create;
begin
  FRootProfile := TProfile.Create('', 0, Nil);
end;

(**

  This is a destructor for the TProfiler class.

  @precon  None.
  @postcon Frees the memory used by the profile tree.

**)
destructor TProfiler.Destroy;
begin
  DumpProfileInformation;
  FRootProfile.Free;
  Inherited Destroy;
end;

(**

  This method starts the process of calculating and outputting the profile tree.

  @precon  None.
  @postcon Starts the process of calculating and outputting the profile tree.

**)
procedure TProfiler.DumpProfileInformation;

Var
  strBuffer : Array[0..MAX_PATH] Of Char;
  strModuleFileName, strFileName : String;
  slProfile: TStringList;

Begin
  GetModuleFileName(hInstance, strBuffer, MAX_PATH);
  strFileName := StrPas(strBuffer);
  strModuleFileName := strFileName;
  strFileName := ChangeFileExt(strFileName, '.profile');
  slProfile := TStringList.Create;
  Try
    slProfile.Add('Profile Dump For Application ' + strModuleFileName + ' on ' +
      FormatDateTime('ddd dd/mmm/yyyy @ hh:mm:ss', Now));
    slProfile.Add(Format('%s,%s,%s,%s,%s,%s,%s,%s', [
      'Stack Depth',
      'Class',
      'Method Name',
      'Total Tick Count (ms)',
      'In Process Tick Count (ms)',
      'Call Count',
      'Average Total Tick Count (ms) per Call',
      'Average In Process Tick Count (ms) per Call'
    ]));
    FRootProfile.DumpProfileInformation(slProfile);
    slProfile.SaveToFile(strFileName);
  Finally
    slProfile.Free;
  End;
end;

(**

  This method stops the profiling of the current method.

  @precon  None.
  @postcon Stops the profiling of the current method.

**)
Procedure TProfiler.Stop;

begin
  FStackTop := FStackTop - 1;
  If FStackTop < 0 Then
    FStackTop := 0;
  FCurrentProfile := FCurrentProfile.StopProfile;
end;

(**

  This method starts the profiling of the current method.

  @precon  None.
  @postcon Starts the profiling of the current method.

  @param   strMethodName as a String

**)
Procedure TProfiler.Start(strMethodName : String);

begin
  If FStackTop = 0 Then
    FCurrentProfile := FRootProfile;
  FStackTop := FStackTop + 1;
  FCurrentProfile := FCurrentProfile.StartProfile(strMethodName, FStackTop);
end;

Initialization
  CodeProfiler := TProfiler.Create;
Finalization
  CodeProfiler.Free;
End.
