(**

  This module contains an OTA interface to install options frames into the IDEs options
  dialogue. Each options frame must implement the IBADIOptionsFrame interface for this
  modules code to load and save their settings.

  @Author  David Hoyle
  @Version 1.0
  @Date    09 Apr 2017

**)
Unit BADI.IDEOptionsHandler;

Interface

Uses
  Classes,
  ToolsAPI,
  Forms,
  BADI.Types,
  BADI.CustomOptionsFrame;

{$INCLUDE CompilerDefinitions.inc}


Type
  (** A class which implements the INTAAddingOptions interface to added options frames
      to the IDEs options dialogue. **)
  TBADIIDEOptionsHandler = Class(TInterfacedObject, INTAAddInOptions)
  Strict Private
    FBADICustomFrameClass : TFrameClass;
    FBADICustomFrame      : TCustomFrame;
    FTitle                : String;
  Strict Protected
    Procedure DialogClosed(Accepted: Boolean); Virtual;
    Procedure FrameCreated(AFrame: TCustomFrame); Virtual;
    Function GetArea: String;
    Function GetCaption: String;
    Function GetFrameClass: TCustomFrameClass;
    Function GetHelpContext: Integer;
    Function IncludeInIDEInsight: Boolean;
    Function ValidateContents: Boolean;
  Public
    Constructor Create(OptionsFrame: TFrameClass; Const strTitle : String); Overload;
  End;

  (** This is a specialisation of the above class specifically for the shortcut frame so that
      a call back mechanism for checking whether shortcuts are in use can be setup. **)
  TBADIIDEShortcutOptionsHandler = Class(TBADIIDEOptionsHandler)
  Strict Private
    FUpdateEvent  : TNotifyEvent;
    FShortcutUsed : TBADIShortcutUsedEvent;
  Strict Protected
    Procedure DialogClosed(Accepted: Boolean); Override;
    Procedure FrameCreated(AFrame: TCustomFrame); Override;
  Public
    Constructor Create(OptionsFrame: TFrameClass; Const strTitle : String;
      UpdateEvent : TNotifyEvent; ShortcutUsed : TBADIShortcutUsedEvent); Overload;
  End;

Implementation

Uses
  SysUtils;

{ TBADIIDEOptionsHandler }

(**

  A constructor for the TBADIIDEOptionsHandler class.

  @precon  None.
  @postcon Saves the options frame title and frame class to later use.

  @param   OptionsFrame as a TFrameClass
  @param   strTitle     as a String as a constant

**)
Constructor TBADIIDEOptionsHandler.Create(OptionsFrame: TFrameClass; Const strTitle : String);

Begin
  FBADICustomFrameClass := OptionsFrame;
  FTitle := strTitle;
End;

(**

  This method is called by the IDE when the IDEs options dialogue is closed.

  @precon  None.
  @postcon If the dialogue was accepted and the frame supports the interface then it saves
           the frame settings.

  @param   Accepted as a Boolean

**)
Procedure TBADIIDEOptionsHandler.DialogClosed(Accepted: Boolean);

Var
  BADIOptionsFrame : IBADIOptionsFrame;

Begin
  If Accepted Then
    Begin
      If Supports(FBADICustomFrame, IBADIOptionsFrame, BADIOptionsFrame) Then
        BADIOptionsFrame.SaveSettings;
    End;
End;

(**

  This method is called by the IDe when the frame is created.

  @precon  None.
  @postcon If the frame supports the interface its settings are loaded.

  @param   AFrame as a TCustomFrame

**)
Procedure TBADIIDEOptionsHandler.FrameCreated(AFrame: TCustomFrame);

Var
  BADIOptionsFrame : IBADIOptionsFrame;

Begin
  FBADICustomFrame := AFrame;
  If Supports(FBADICustomFrame, IBADIOptionsFrame, BADIOptionsFrame) Then
    BADIOptionsFrame.LoadSettings;
End;

(**

  This is a getter method for the Area property.

  @precon  None.
  @postcon Called by the IDE. NULL string is returned to place the options frame under the
           third party node.

  @return  a String

**)
Function TBADIIDEOptionsHandler.GetArea: String;

Begin
  Result := '';
End;

(**

  This is a getter method for the Caption property.

  @precon  None.
  @postcon This is called by the IDe to get the caption of the options frame in the IDEs
           options dialogue in the left treeview.

  @return  a String

**)
Function TBADIIDEOptionsHandler.GetCaption: String;

Begin
  If FTitle <> '' Then
    Result := Format('Browse and Doc It.%s', [FTitle])
  Else
    Result := 'Browse and Doc It';
End;

(**

  This is a getter method for the FrameClass property.

  @precon  None.
  @postcon This is called by the IDE to get the frame class to create when displaying the
           options dialogue.

  @return  a TCustomFrameClass

**)
Function TBADIIDEOptionsHandler.GetFrameClass: TCustomFrameClass;

Begin
  Result := FBADICustomFrameClass;
End;

(**

  This is a getter method for the HelpContext property.

  @precon  None.
  @postcon This is called by the IDe and returns 0 to signify no help.

  @return  an Integer

**)
Function TBADIIDEOptionsHandler.GetHelpContext: Integer;

Begin
  Result := 0;
End;

(**

  This is called by the IDE to determine whether the controls on the options frame are
  displayed in the IDE Insight search.

  @precon  None.
  @postcon Returns true to be include in IDE Insight.

  @return  a Boolean

**)
Function TBADIIDEOptionsHandler.IncludeInIDEInsight: Boolean;

Begin
  Result := True;
End;

(**

  This method is called by the IDE to validate the frame.

  @precon  None.
  @postcon Not used so returns true.

  @return  a Boolean

**)
Function TBADIIDEOptionsHandler.ValidateContents: Boolean;

Begin
  Result := True;
End;

{ TBADIIDEShortcutOptionsHandler }

(**

  A constructor for the TBADIIDEShortcutOptionsHandler class.

  @precon  None.
  @postcon Initalises the class and saves references to a Notify event and a call back event.

  @param   OptionsFrame as a TFrameClass
  @param   strTitle     as a String as a constant
  @param   UpdateEvent  as a TNotifyEvent
  @param   ShortcutUsed as a TBADIShortcutUsedEvent

**)
Constructor TBADIIDEShortcutOptionsHandler.Create(OptionsFrame: TFrameClass; Const strTitle: String;
  UpdateEvent: TNotifyEvent; ShortcutUsed : TBADIShortcutUsedEvent);

Begin
  Inherited Create(OptionsFrame, strTitle);
  FUpdateEvent := UpdateEvent;
  FShortcutUsed := ShortcutUsed;
End;

(**

  This method is called when the Options frame is about to be closed.

  @precon  None.
  @postcon If an event handlder is assigned to update the actions it is called.

  @param   Accepted as a Boolean

**)
Procedure TBADIIDEShortcutOptionsHandler.DialogClosed(Accepted: Boolean);

Begin
  Inherited DialogClosed(Accepted);
  If Accepted Then
    If Assigned(FUpdateEvent) Then
      FUpdateEvent(Self);
End;

(**

  This method is called when the Options frame has just been created.

  @precon  None.
  @postcon If the interface is supported the call back is installed.

  @param   AFrame as a TCustomFrame

**)
Procedure TBADIIDEShortcutOptionsHandler.FrameCreated(AFrame: TCustomFrame);

Var
  I : IBADIInstallShortcutUsedCallBack;

Begin
  Inherited FrameCreated(AFrame);
  If Supports(AFrame, IBADIInstallShortcutUsedCallBack, I) Then
    I.InstallShortcutUsedCallBack(FShortcutUsed);
End;

End.
