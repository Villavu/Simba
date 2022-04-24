pushSection('Classes');

addClass('TMMLTimer');
addClassVar('TMMLTimer', 'Enabled', 'Boolean', @_LapeMMLTimer_Enabled_Read, @_LapeMMLTimer_Enabled_Write);
addClassVar('TMMLTimer', 'Interval', 'Integer', @_LapeMMLTimer_Interval_Read, @_LapeMMLTimer_Interval_Write);
addClassVar('TMMLTimer', 'OnTimer', 'TNotifyEvent', @_LapeMMLTimer_OnTimer_Read, @_LapeMMLTimer_OnTimer_Write);
addGlobalFunc('procedure TMMLTimer.On; constref', @_LapeMMLTimer_On);
addGlobalFunc('procedure TMMLTimer.Off; constref', @_LapeMMLTimer_Off);
addGlobalFunc('procedure TMMLTimer.Init()', @_LapeMMLTimer_Init);
//addGlobalFunc('procedure TMMLTimer.Free; constref', @_LapeMMLTimer_Free);

popSection();
