(**

  This module contains graphics control which provide information on the
  status of the battery in the computer.

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Apr 2011

**)
Unit DGHMemoryMonitorControl;

Interface

  Uses
    SysUtils, Classes, ExtCtrls, Windows, Messages, Graphics,
    DGHCustomGraphicsControl;

  Type
    (** A class to represent the Battery Monitor Grasphics control. **)
    TDGHMemoryMonitor = Class(TDGHCustomGraphicControl)
    Private
      { Private declarations }
      FTimer             : TTimer;
      FUpdateInterval    : TInterval;
      FUsed              : Cardinal;
      FReserved          : Cardinal;
      FPercentage        : TPercentage;
      FFullColour        : TColor;
      FHalfColour        : TColor;
      FCriticalColour    : TColor;
      FFullFontColour    : TColor;
      FHalfFontColour    : TColor;
      FCriticalFontColour: TColor;
      FBackColour        : TColor;
      FFullPoint         : TPercentage;
      FCriticalPoint     : TPercentage;
      FHalfPoint         : TPercentage;
      FBackFontColour    : TColor;
    Protected
      { Protected declarations }
      Procedure Paint; Override;
      Procedure SetUpdateInterval(Value: TInterval); Virtual;
      Procedure SetCriticalPoint(Const Value: TPercentage); Virtual;
      Procedure SetFullPoint(Const Value: TPercentage); Virtual;
      Procedure SetHalfPoint(Const Value: TPercentage); Virtual;
      Procedure UpdateControl(Sender: TObject); Virtual;
      Procedure GetBatteryState; Virtual;
    Public
      { Public declarations }
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
    Published
      { Published declarations }
      (**
       This property gets or sets the timing interval between information updates.
       @precon  None.
       @postcon Gets or sets the timing interval between information updates.
       @return  an TInterval
       **)
      Property UpdateInterval
        : TInterval Read FUpdateInterval Write SetUpdateInterval Default 5;
      (**
       This property gets or set the back ground colour of the control.
       @precon  None.
       @postcon Gets or set the back ground colour of the control.
       @return  a TColor
       **)
      Property BackColour
        : TColor Read FBackColour Write FBackColour Default clGray;
      (**
       This property gets or set the back ground font colour of the control.
       @precon  None.
       @postcon Gets or set the back ground font colour of the control.
       @return  a TColor
       **)
      Property BackFontColour
        : TColor Read FBackFontColour Write FBackFontColour
        Default clBlack;
      (**
       This property gets or set the Full Battery colour of the control.
       @precon  None.
       @postcon Gets or set the Full Battery colour of the control.
       @return  a TColor
       **)
      Property FullColour
        : TColor Read FFullColour Write FFullColour Default clLime;
      (**
       This property gets or set the Half Empty Battery colour of the control.
       @precon  None.
       @postcon Gets or set the Half Empty Battery colour of the control.
       @return  a TColor
       **)
      Property HalfColour
        : TColor Read FHalfColour Write FHalfColour Default $00CCFF;
      (**
       This property gets or set the Critical Battery colour of the control.
       @precon  None.
       @postcon Gets or set the Critical Battery colour of the control.
       @return  a TColor
       **)
      Property CriticalColour
        : TColor Read FCriticalColour Write FCriticalColour
        Default clRed;
      (**
       This property gets or set the Full Battery Font colour of the control.
       @precon  None.
       @postcon Gets or set the Full Battery Font colour of the control.
       @return  a TColor
       **)
      Property FullFontColour
        : TColor Read FFullFontColour Write FFullFontColour
        Default clBlack;
      (**
       This property gets or set the Half Empty Battery Font colour of the control.
       @precon  None.
       @postcon Gets or set the Half Empty Battery Font colour of the control.
       @return  a TColor
       **)
      Property HalfFontColour
        : TColor Read FHalfFontColour Write FHalfFontColour
        Default clBlack;
      (**
       This property gets or set the Critical Battery Font colour of the control.
       @precon  None.
       @postcon Gets or set the Critical Battery Font colour of the control.
       @return  a TColor
       **)
      Property CriticalFontColour
        : TColor Read FCriticalFontColour Write FCriticalFontColour
        Default clBlack;
      (**
       This property gets or set the Critical Battery Point (Percentage) of the control.
       @precon  None.
       @postcon Gets or set the Critical Battery Point (Percentage) of the control.
       @return  a TPercentage
       **)
      Property CriticalPoint
        : TPercentage Read FCriticalPoint Write SetCriticalPoint Default
        10;
      (**
       This property gets or set the Half Empty Battery Point (Percentage) of the control.
       @precon  None.
       @postcon Gets or set the Half Empty Battery Point (Percentage) of the control.
       @return  a TPercentage
       **)
      Property HalfPoint
        : TPercentage Read FHalfPoint Write SetHalfPoint Default 50;
      (**
       This property gets or set the Full Battery Point (Percentage) of the control.
       @precon  None.
       @postcon Gets or set the Full Battery Point (Percentage) of the control.
       @return  a TPercentage
       **)
      Property FullPoint
        : TPercentage Read FFullPoint Write SetFullPoint Default 90;
      (** Publish this existing property. @precon  None. @postcon None. **)
      Property Align;
      (** Publish this existing property. @precon  None. @postcon None. **)
      Property BevelInner;
      (** Publish this existing property. @precon  None. @postcon None. **)
      Property BevelOuter;
      (** Publish this existing property. @precon  None. @postcon None. **)
      Property BevelWidth;
      (** Publish this existing property. @precon  None. @postcon None. **)
      Property BorderWidth;
      (** Publish this existing property. @precon  None. @postcon None. **)
      Property Font;
    End;

  Procedure Register;

Implementation

  Uses
    Types, dghlibrary;

  (**

   This procedure registers the component with the BDS IDE.

   @precon  None.
   @postcon Registers the component with the BDS IDE.

   **)
  Procedure Register;
  Begin
    RegisterComponents('DGH Controls', [TDGHMemoryMonitor]);
  End;

  (**

    This function calculates a shorter representation of a memory size and
    returns a string representing the value.

    @precon  None.
    @postcon Returns a string representation of the given size.

    @param   iSize as a Cardinal
    @return  a String

  **)
  Function CalcSize(iSize : Cardinal) : String;

  Const
    dblKILOBYTE : Double = 1024.0;
    dblMEGABYTE : Double = 1024.0 * 1024.0;

  Begin
    Result := 'Unknown';
    If iSize < dblKILOBYTE Then
      Result := Format('%1.0n', [Int(iSize)])
    Else If iSize < dblMEGABYTE Then
      Result := Format('%1.2nK', [Int(iSize) / dblKILOBYTE])
    Else
      Result := Format('%1.2nM', [Int(iSize) / dblMEGABYTE])
  End;

  { TDGHBatteryMonitor }

  (**

   This is the constructor method for the TDGHBatteryMonitor class.

   @precon  None.
   @postcon Creates and initialise the component including the timer.

   @param   AOwner as a TComponent

   **)
  Constructor TDGHMemoryMonitor.Create(AOwner: TComponent);
  Begin
    Inherited Create(AOwner);
    FUpdateInterval := 5;
    FBackColour := clGray;
    FBackFontColour := clBlack;
    FFullColour := clLime;
    FHalfColour := $00CCFF;
    FCriticalColour := clRed;
    FFullFontColour := clBlack;
    FHalfFontColour := clBlack;
    FCriticalFontColour := clBlack;
    FCriticalPoint := 10;
    FHalfPoint := 50;
    FFullPoint := 90;
    Width := 125;
    Height := 22;
    FTimer := TTimer.Create(Nil);
    FTimer.Interval := FUpdateInterval * 1000;
    FTimer.OnTimer := UpdateControl;
    FTimer.Enabled := True;
    UpdateControl(Nil);
  End;

  (**

   This is the destructor method for the TDGHBatteryMonitor class.

   @precon  None.
   @postcon Disabled the timer and free's its mmemory.

   **)
  Destructor TDGHMemoryMonitor.Destroy;
  Begin
    FTimer.Enabled := False;
    FTimer.Free;
    Inherited Destroy;
  End;

  (**

   This method sets the interval variables with information about the systems
   power status.

   @precon  None.
   @postcon FBatteryStatus, FPercentage and FRemainingTime are set with the
   battery status.

   **)
  Procedure TDGHMemoryMonitor.GetBatteryState;

  Var
    MMS : TMemoryManagerState;
    i : Integer;
    SBTS: TSmallBlockTypeState;

  Begin
    GetMemoryManagerState(MMS);
    FUsed := MMS.TotalAllocatedLargeBlockSize + MMS.TotalAllocatedMediumBlockSize;
    FReserved := MMS.ReservedLargeBlockAddressSpace + MMS.ReservedMediumBlockAddressSpace;
    For i := Low(MMS.SmallBlockTypeStates) To High(MMS.SmallBlockTypeStates) Do
      Begin
        SBTS := MMS.SmallBlockTypeStates[i];
        Inc(FUsed, SBTS.UseableBlockSize * SBTS.AllocatedBlockCount);
        Inc(FReserved, SBTS.ReservedAddressSpace);
      End;
    FPercentage := 0;
    If FReserved > 0 Then
      FPercentage := Trunc(Int(FUsed) / Int(FReserved) * 100);
  End;

  (**

   This is the controls overridden paint method.

   @precon  None.
   @postcon Draw the display of battery information.

   **)
  Procedure TDGHMemoryMonitor.Paint;

  Const
    iMargin : Integer  = 8;

  Var
    strStatus : String;
    R, T: TRect;
    strU, strR : String;

  Begin
    Inherited Paint;
    Canvas.Font.Assign(Font);
    R := ClientRect;
    // Render Background
    Canvas.Brush.Color := FBackColour;
    Canvas.FillRect(R);
    Canvas.Font.Color := FBackFontColour;
    // Write Background text
    strU := CalcSize(FUsed);
    strR := CalcSize(FReserved);
    strStatus := Format('Used %s in %s bytes (%1.1n%%)', [strU, strR, Int(FPercentage)]);
    If Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
      strStatus := Format('%s in %s (%1.1n%%)', [strU, strR, Int(FPercentage)]);
    If Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
      strStatus := Format('%s (%1.1n%%)', [strU, Int(FPercentage)]);
    T.Left := R.Left + ((R.Right - R.Left) - Canvas.TextWidth(strStatus)) Div 2;
    T.Right := T.Left + Canvas.TextWidth(strStatus);
    T.Top := R.Top;
    T.Bottom := R.Bottom;
    DrawText(Canvas.Handle, PChar(strStatus), Length(strStatus), T,
      DT_SINGLELINE Or DT_VCENTER);
    // Render % of memory full in custom colour
    If FPercentage In [0 .. 100] Then
      Begin
        Canvas.Brush.Color := CalcColour(FPercentage, FCriticalPoint,
          FHalfPoint, FFullPoint, FFullColour, FHalfColour,
          FCriticalColour);
        R.Right := R.Left + (R.Right - R.Left) * FPercentage Div 100;
        Canvas.FillRect(R);
      End
    Else
      Begin
        Canvas.Brush.Color := FCriticalColour;
        Canvas.FillRect(R);
      End;
    // Render % of memory text.
    T.Right := R.Right;
    Canvas.Font.Color := CalcColour(FPercentage, FCriticalPoint, FHalfPoint,
      FFullPoint, FFullFontColour, FHalfFontColour, FCriticalFontColour);
    If T.Right >= T.Left Then
      DrawText(Canvas.Handle, PChar(strStatus), Length(strStatus), T,
        DT_SINGLELINE Or DT_VCENTER);
  End;

  (**

   This is a setter method for the CriticalPoint property.

   @precon  None.
   @postcon Sets the CriticalPoitn property.

   @param   Value as a TPercentage as a constant

   **)
  Procedure TDGHMemoryMonitor.SetCriticalPoint(Const Value: TPercentage);
  Begin
    If (Value < FHalfPoint) And (Value > 0) Then
      FCriticalPoint := Value;
  End;

  (**

   This is a setter method for the FullPoint property.

   @precon  None
   @postcon Sets the FullPoint property.

   @param   Value as a TPercentage as a constant

   **)
  Procedure TDGHMemoryMonitor.SetFullPoint(Const Value: TPercentage);
  Begin
    If (Value > FHalfPoint) And (Value <= 100) Then
      FFullPoint := Value;
  End;

  (**

   This is a setter method for the HalfPoint property.

   @precon  None.
   @postcon Sets the HalfPoint Property

   @param   Value as a TPercentage as a constant

   **)
  Procedure TDGHMemoryMonitor.SetHalfPoint(Const Value: TPercentage);
  Begin
    If (Value > FCriticalPoint) And (Value < FFullPoint) Then
      FHalfPoint := Value;
  End;

  (**

   This is a setter method for the UpdateInterval property.

   @precon  None.
   @postcon Set the update interval of the componenot and the timer control.

   @param   Value as an TInterval

   **)
  Procedure TDGHMemoryMonitor.SetUpdateInterval(Value: TInterval);
  Begin
    FUpdateInterval := Value;
    FTimer.Interval := Value * 1000;
  End;

  (**

   This method is an on timer event handler for the timer control.

   @precon  None.
   @postcon Gets the battery status and then forces the control to re-paint.

   @param   Sender as a TObject

   **)
  Procedure TDGHMemoryMonitor.UpdateControl(Sender: TObject);
  Begin
    GetBatteryState;
    If CanInvalidate Then
      Invalidate;
  End;

End.
