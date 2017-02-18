(**

  This module contains a class to represent module information stored in the Modules Dispatcher.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.ModuleInfo;

Interface

Uses
  BADI.Base.Module,
  BADI.Types;

Type
  (** A class to hold the information required by the system for each registered module
      parser. **)
  TModuleInfo = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FExt: String;
    FCls: TBaseLanguageModuleClass;
    FCanDoc: Boolean;
    FBlockCmt: TCommentType;
    FLineCmt: TCommentType;
    FInSituCmt: TCommentType;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(const strExt: String; Cls: TBaseLanguageModuleClass; boolCanDoc: Boolean;
      iBlockCmt, iLineCmt, iInSituCmt: TCommentType);
    (**
      This property returns the extension associated with the registration.
      @precon  None.
      @postcon Returns the extension associated with the registration.
      @return  a String
    **)
    Property Ext: String Read FExt;
    (**
      This property returns the class reference for this registration.
      @precon  None.
      @postcon Returns the class reference for this registration.
      @return  a TBaseLanguageModuleClass
    **)
    Property Cls: TBaseLanguageModuleClass Read FCls;
    (**
      This property returns whether the registration supports documentation.
      @precon  None.
      @postcon Returns whether the registration supports documentation.
      @return  a Boolean
    **)
    Property CanDoc: Boolean Read FCanDoc;
    (**
      This property returns the comment type for blocks.
      @precon  None.
      @postcon Returns the comment type for blocks.
      @return  a TCommentType
    **)
    Property BlockCmt: TCommentType Read FBlockCmt;
    (**
      This property returns the comment type for lines.
      @precon  None.
      @postcon Returns the comment type for lines.
      @return  a TCommentType
    **)
    Property LineCmt: TCommentType Read FLineCmt;
    (**
      This property returns the comment type for In Situ.
      @precon  None.
      @postcon Returns the comment type for In Situ.
      @return  a TCommentType
    **)
    Property InSituCmt: TCommentType Read FInSituCmt;
  End;

Implementation

(**

  A constructor for the TModuleInfo class.

  @precon  None.
  @postcon Initialises the class with information.

  @param   strExt     as a String as a constant
  @param   Cls        as a TBaseLanguageModuleClass
  @param   boolCanDoc as a Boolean
  @param   iBlockCmt  as a TCommentType
  @param   iLineCmt   as a TCommentType
  @param   iInSituCmt as a TCommentType

**)
Constructor TModuleInfo.Create(const strExt: String; Cls: TBaseLanguageModuleClass; boolCanDoc: Boolean;
  iBlockCmt, iLineCmt, iInSituCmt: TCommentType);

Begin
  FExt := strExt;
  FCls := Cls;
  FCanDoc := boolCanDoc;
  FBlockCmt := iBlockCmt;
  FLineCmt := iLineCmt;
  FInSituCmt := iInSituCmt;
End;

End.
