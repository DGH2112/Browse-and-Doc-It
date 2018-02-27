unit uSCMConst;

interface

const
  // Database/model version info.
  REQ_DB_VERSION = 49;
  REQ_MODEL_VERSION = 14;

  MIN_DB_VERSION = 40;
  MIN_MODEL_VERSION = 0;

  // Database Settings Constants
  DSC_CALL_NUMBER = 1;
  DSC_MAIL_SETTINGS = 2;
  DSC_GENERAL_SETTINGS = 3;
  DSC_DB_VERSION = 4;
  DSC_ACTIONMONITOR = 5;
  DSC_MODEL_VERSION = 6;

  // Company Info
  MainCompanyName         = 'Welcome Customer Care Solutions';
  MainApplicationName     = 'SCM';
  MainApplicationFullName = 'Welcome SCM';
  MainCompanyAddress      = 'D.J. Jittastraat 2b';
  MainCompanyZip          = '5042 MX Tilburg';
  MainCompanyPhone        = '+31(0)13-571 88 44';
  MainCompanyFax          = '+31(0)13-571 88 43';
  MainCompanyMail         = 'info@welcomeccs.nl';
  MainCompanyWebsite      = 'http://www.welcomeccs.nl';
  // Application Info
  SCMRegKey               = '\SOFTWARE\Welcome CCS\SCM';
  SCMActiveDBKey          = 'ActiveDB';
  SCMWorkDays             = 5;
  SCMStartOfDay           = '8:00';
  SCMEndOfDay             = '17:00';

  // StateSettings
  psSectionName             = 'StateSection';
  psStateCallOpen           = 'CallOpen';
  psStateCallFontOpen       = 'CallOpenFont';
  psStateCallWarning        = 'CallWarning';
  psStateCallFontWarning    = 'CallWarningFont';
  psStateCallToday          = 'CallToday';
  psStateCallFontToday      = 'CallTodayFont';
  psStateCallOverdue        = 'CallOverdue';
  psStateCallFontOverdue    = 'CallOverdueFont';
  psStateCallFinished       = 'CallFinished';
  psStateCallFontFinished   = 'CallFinishedFont';
  psStateCallDeleted        = 'CallDeleted';
  psStateCallFontDeleted    = 'CallDeletedFont';
  psStateCallWarningDays    = 'CallWarningDays';
  psStateCallOnHold         = 'CallOnHold';
  psStateCallFontOnHold     = 'CallOnHoldFont';
  psStateActionOpen         = 'ActionOpen';
  psStateActionFontOpen     = 'ActionOpenFont';
  psStateActionWarning      = 'ActionWarning';
  psStateActionFontWarning  = 'ActionWarningFont';
  psStateActionToday        = 'ActionToday';
  psStateActionFontToday    = 'ActionTodayFont';
  psStateActionOverdue      = 'ActionOverdue';
  psStateActionFontOverdue  = 'ActionOverdueFont';
  psStateActionFinished     = 'ActionFinished';
  psStateActionFontFinished = 'ActionFinishedFont';
  psStateActionDeleted      = 'ActionDeleted';
  psStateActionFontDeleted  = 'ActionDeletedFont';
  psStateActionWarningDays  = 'ActionWarningDays';

  // Layout Types
  loBOFrm     = 0;
  loBOSearch  = 1;
  loBOPreview = 2;
  loCallIns   = 3;
  loCallDetail= 4;
  loSideBar   = 5;

  // BO ClassID's
  cidCall       = 1;
  cidAction     = 2;
  cidDIS        = 3;
  cidEmployee   = 4;
  cidDepartment = 5;
  cidRelation   = 6;
  cidCost       = 7;

  // Bevoegdheden
  fnPassword        = 1;
  fnFieldMaint      = 2;
  fnTemplates       = 3;
  fnCallFinish      = 4;
  fnActionFinish    = 5;
  fnAuthentication  = 6;
  fnScreensDefault  = 7;
  fnScreensPersonal = 8;
  fnWorkflow        = 9;
  fnWordTemplates   = 10;
  fnMailModule      = 11;
  fnLog             = 12;
  fnSettingsDefault = 13;
  fnSettingsPersonal= 14;
  fnColumnSettings  = 15;
  fnLayoutDefault   = 16;
  fnLayoutPersonal  = 17;
  fnManagement      = 18;
  fnCallNumber      = 19;
  fnRegistration    = 20;
  fnEmpFupActionFin = 21;
  fnActionMonitor   = 22;
  fnDurationSettings= 23;
  fnOnHold          = 24;
  fnEditClosedCall  = 25;

  DateValueIndexLater         = -8;
  DateValueIndexNextWeek      = -7;
  DateValueIndexNextSunday    = -6;
  DateValueIndexNextSaturday  = -5;
  DateValueIndexNextFriday    = -4;
  DateValueIndexNextThursday  = -3;
  DateValueIndexDayAfter      = -2;
  DateValueIndexTomorrow      = -1;
  DateValueIndexEmpty         =  0;
  DateValueIndexToday         =  1;
  DateValueIndexYesterday     =  2;
  DateValueIndexLastFriday    =  3;
  DateValueIndexLastThursday  =  4;
  DateValueIndexLastWednesday =  5;
  DateValueIndexLastTuesday   =  6;
  DateValueIndexLastMonday    =  7;
  DateValueIndexLastWeek      =  8;
  DateValueIndexTwoWeeksAgo   =  9;
  DateValueIndexOlder         = 10;

  DisplayText: Array[DateValueIndexLater..DateValueIndexOlder] of String =
          ('Later', 'Volgende week', 'Zondag', 'Zaterdag', 'Vrijdag', 'Donderdag',
           'Overmorgen', 'Morgen', 'Leeg', 'Vandaag', 'Gisteren', 'Vrijdag',
           'Donderdag', 'Woensdag', 'Dinsdag', 'Maandag', 'Vorige week',
           'Twee weken geleden', 'Ouder');

  // ProtectedFields are fields that the user cannot change the type of,
  //  only the name of the field can be changed
  ProtectedFields: string =
      ('Call_Number,DatIns,DatFup,DatFin,EmpIns,EmpFup,EmpFin,Status,DIS_FileOrg,'
      +'DIS_MailFrom,DIS_MailTo,Emp_Email,Dep_Email,Act_CallID,DIS_CallID'
      +'EmpChg,DatChg,Act_Memo,Cost_CallID,Cost_Value,Call_Cost');

  // AutopropertyFields are fields that are handeld by the application,
  //  the user cannot change the value of the fields (readonly)
  AutoPropertyFlds: string =
      ('DatIns,EmpIns,DatFin,EmpFin,DatChg,EmpChg,DIS_FileOrg,DIS_MailFrom,'
      +'DIS_MailTo,Call_Cost');

type
  // Used for the checkboxes displayed within a treeview.
  TCheckState  = (csUnchecked = 1, csChecked, csGrayed);

  // The states used in the SCM application 
  TState = (
    stIsOpen,
    stIsWarning,
    stIsDueToday,
    stIsOverDue,
    stIsClosed,
    stActionIsOpen,
    stActionIsWarning,
    stActionIsDueToday,
    stActionIsOverDue,
    stActionIsClosed,
    stOnHold
  );

  TSCMFilter = (
    fiAll,
    fiCallResp,
    fiActionResp,
    fiCallDep,
    fiActionDep,
    fiCallIns,
    fiActionIns
  );
  TSCMFilters = set of TSCMFilter;

  { Possible permissions on functionality and tables }
  TPermission = (
    bhReadExecute,
    bhAdd,
    bhChange,
    bhDelete
  );
  TPermissions = set of TPermission;



const
  { @todo Make this array language independant. }
  StatStr: array[TState] of string = (
    'Open',
    'Waarschuwing',
    'Vandaag',
    'Te laat',
    'Afgewerkt',
    'Actie Open',
    'Actie Waarschuwing',
    'Actie Vandaag',
    'Actie Te laat',
    'Actie Afgewerkt',
    'On-Hold'
  );

implementation

end.

