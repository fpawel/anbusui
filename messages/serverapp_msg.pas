unit serverapp_msg;

interface

type
    TServerAppUserMsg = (msgPeer, msgSetVarChecked, msgSetPlaceChecked,
      msgAddDelVar, msgAddDelPlace, msgSetAddr, msgSetVar, msgToggle);

    TServerAppDataMsg = (dmsgSetsProp, dmsgPerformTextCommand);

implementation

end.
