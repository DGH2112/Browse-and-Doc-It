(**
  
  This module contains a class to manage frames for editor views.

  @Author  David Hoyle
  @Version 1.169
  @Date    28 Aug 2020
  
  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
  
**)
Unit BADI.Frame.Manager;

Interface

Uses
  Vcl.Forms,
  System.Generics.Collections;

Type
  (** A class to manage the frames against the editor windows. **)
  TBADIFrameManager = Class
  Strict Private
    Type
      (** A record to stored information about each view. **)
      TBADIFrameManagerRecord  = Record
        FEditWindowName : String;
        FFrameReference : Tframe;
      End;
  Strict Private
    FFrames : TList<TBADIFrameManagerRecord>;
  Strict Protected
    Function  GetFrame(Const strEditWindowName : String) : Tframe;
    Function  Find(Const strEditWindowName :String) : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Const strEditWindowName : String; Const AFrame : Tframe);
    (**
      A property to returned the frame associated with the given edit window.
      @precon  None.
      @postcon Returned the frame associated with the given edit window.
      @param   strEditWindowName as a String as a constant
      @return  a Tframe
    **)
    Property Frame[Const strEditWindowName : String] : Tframe Read GetFrame;
  End;

Implementation

Uses
  System.SysUtils;

(**

  This method adds the given edit window name and frame reference pair to the collection is it does not 
  already exists else it updates the existing records frame reference.

  @precon  AFrame must be a valid instance.
  @postcon Either a new reference is added to the collection if it does not exist else the existing 
           reference is updated.

  @param   strEditWindowName as a String as a constant
  @param   AFrame            as a Tframe as a constant

**)
Procedure TBADIFrameManager.Add(Const strEditWindowName: String; Const AFrame: Tframe);

Var
  iIndex: Integer;
  R: TBADIFrameManagerRecord;

Begin
  iIndex := Find(strEditWindowName);
  If iIndex = -1 Then
    Begin // Create new view
      R.FEditWindowName := strEditWindowName;
      R.FFrameReference := AFrame;
      FFrames.Add(R);
    End Else
    Begin // Update existing reference
      R := FFrames[iIndex];
      R.FFrameReference := AFrame;
      FFrames[iIndex] := R;
    End;
End;

(**

  A constructor for the TBADIFrameManager class.

  @precon  None.
  @postcon Creates an empty collection.

**)
Constructor TBADIFrameManager.Create;

Begin
  FFrames := TList<TBADIFrameManagerRecord>.Create;
End;

(**

  A destructor for the TBADIFrameManager class.

  @precon  None.
  @postcon Frees the collection.

**)
Destructor TBADIFrameManager.Destroy;

Begin
  FFrames.Free;
  Inherited Destroy;
End;

(**

  This method attempts to find the given view name in the collection and if found returns the index
  else returns -1 for not found.

  @precon  None.
  @postcon Returns the index of the named view if found else -1.

  @param   strEditWindowName as a String as a constant
  @return  an Integer

**)
Function TBADIFrameManager.Find(Const strEditWindowName: String): Integer;

Var
  iView: Integer;

Begin
  Result := -1;
  For iView := 0 To FFrames.Count - 1 Do
    If CompareText(FFrames[iView].FEditWindowName, strEditWindowName) = 0 Then
      Begin
        Result := iView;
        Break;
      End;
End;

(**

  This is a getter method for the Frame property.

  @precon  None.
  @postcon If the named edit window is found in the collection then the associated frame is returned 
           else nil is returned for not found.

  @param   strEditWindowName as a String as a constant
  @return  a Tframe

**)
Function TBADIFrameManager.GetFrame(Const strEditWindowName: String): Tframe;

Var
  iIndex: Integer;
  
Begin
  Result := Nil;
  iIndex := Find(strEditWindowName);
  If iIndex > -1 Then
    Result := FFrames[iiNdex].FFrameReference;
End;

End.
