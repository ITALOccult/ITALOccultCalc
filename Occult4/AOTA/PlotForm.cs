#define TRACE
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using LightCurves;
using Occult;
using Occult.Asteroid_Observations;
using Occult.File_Actions;
using Occult.Properties;

namespace AOTA
{
	public class PlotForm : Form
	{
		internal double OldScaleValue;

		private bool IsLoading;

		internal RadioButton[] radioAddEvent = (RadioButton[])(object)new RadioButton[5];

		internal CheckBox[] chkDisplayEvent = (CheckBox[])(object)new CheckBox[5];

		internal TextBox[] txtPositions = (TextBox[])(object)new TextBox[5];

		internal TextBox[] txtWidths = (TextBox[])(object)new TextBox[5];

		internal Button[] cmdEvaluate = (Button[])(object)new Button[5];

		internal CheckBox[] chkSetAsMiss = (CheckBox[])(object)new CheckBox[5];

		internal int IndexOfEventToUpdate;

		internal static List<CameraDelays> Delays = new List<CameraDelays>();

		private CameraDelays CameraDelays;

		private Timer T = new Timer();

		private Timer Tcopy = new Timer();

		internal static double CameraDelaySecs = 0.0;

		public bool RunningInDevelopmentEnvironment;

		private bool ShowToolTip = true;

		private bool GetToolTip = true;

		private int RunningAverageCount = 25;

		private string LightCurveMessage = "The tool for reporting the light curve includes the following options which can change the data used for analysing the light curve for event times.\r\n* Change Integration/binning or normalisation\r\n* Insert, delete or invalidate frames in the Integrity check\r\n\r\nAs a result, you should only use the functionality to report the light curve when you have completed the analysis of the light curve for event times.\r\n\r\nDo you want to continue with reporting the Light Curve?";

		private bool CameraChanging;

		private IContainer components;

		internal PictureBox picPlot;

		private MenuStrip menuStrip1;

		internal Panel panelPlot;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openCSVFileToolStripMenuItem;

		internal NumericUpDown updnScale;

		internal CheckBox chkComp1;

		internal CheckBox chkComp2;

		private RadioButton optBeforeD;

		private RadioButton optAfterD;

		private RadioButton optBeforeR;

		private RadioButton optAfterR;

		internal TextBox txtBeforeD;

		internal TextBox txtAfterD;

		internal TextBox txtAfterR;

		internal TextBox txtBeforeR;

		internal TextBox txtSN_at_R;

		internal TextBox txtSN_at_D;

		private Label label4;

		private Label label3;

		internal TabPage tabPage6;

		internal TextBox txtFullLightLevel;

		internal TextBox txtFullPosn;

		internal TextBox txtOccLevelPos;

		internal TextBox txtOccultedLightLevel;

		private Label label9;

		private Label label8;

		private Label label13;

		private Label label12;

		private Label label11;

		private Label label10;

		internal TabPage tabPage4;

		internal TabControl tabPlot;

		private Label label15;

		internal TextBox txtMaxFrames;

		private NumericUpDown updnMaxDurn;

		private Label label14;

		private Button cmdFindPossible;

		internal ProgressBar pBar;

		internal Label lblCurrentWidth;

		private Panel panelTab1;

		private Panel panelTab2;

		private Panel panelTab4;

		private Panel panelTab3;

		private Panel panelEvents;

		private Label label19;

		private Label label18;

		private Label label17;

		private Label label16;

		private GroupBox groupBox1;

		private Label label2;

		private GroupBox grpExpected;

		internal TextBox txtMagDropLightLevel;

		private Label label20;

		private GroupBox grpMaxDurn;

		internal NumericUpDown updnMagDrop;

		internal NumericUpDown updnMagObject;

		internal NumericUpDown updnMagStar;

		internal Button cmdCancelFind;

		internal RadioButton optPAL;

		internal RadioButton optNTSC;

		internal ComboBox cmbIntegration;

		private Label label21;

		private Label label22;

		internal TextBox txtExpectedSN;

		internal RadioButton optPossOccLevel;

		internal RadioButton optFullLight;

		internal PictureBox picChiR;

		internal PictureBox picChiD;

		private Label label23;

		private Label label25;

		private Label label24;

		private ToolStripMenuItem exitToolStripMenuItem;

		internal NumericUpDown updnExtraPoints;

		private Button cmdGetDandR;

		private Label label26;

		private Button cmdTest;

		private Label lblRframe;

		private Label lblDframe;

		internal PictureBox picRUncert;

		internal PictureBox picDUncert;

		private Label label27;

		private Panel panelSN;

		private Label label28;

		private Panel panelAdjustRegions;

		private Label label29;

		private Label label30;

		internal NumericUpDown updnMonteCarlo;

		private Label label31;

		private Label label32;

		internal NumericUpDown updnTransitionNumber;

		private Label label33;

		internal NumericUpDown updnSDLimit;

		private Label label34;

		private Panel panelChi2;

		private Label label36;

		private Label label35;

		private Panel panelMonteCarlo;

		private Panel panelGetLocations;

		private Label label37;

		private Button cmd_x10;

		private Button cmd_x15;

		private Button cmd_x5;

		private Button cmd_x1;

		private Label label38;

		private Label label1;

		private Panel panelUncertainty;

		private Panel panelTransitions;

		private Panel panelDR;

		private Panel panelFind;

		private Panel panelComp;

		internal CheckBox chkShowBackground;

		private Button cmdOpen;

		private Label label39;

		internal NumericUpDown updnVerticalScaleAdjustment;

		internal CheckBox chkComp3;

		private ToolStripMenuItem savePlotToolStripMenuItem;

		private ToolStripMenuItem saveFormAsAnImageToolStripMenuItem;

		private ToolStripMenuItem editToolStripMenuItem;

		private ToolStripMenuItem copyPlotToolStripMenuItem;

		private ToolStripMenuItem copyFormToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private Label label41;

		private Label label40;

		internal CheckBox chkShowCrossCorr;

		private ToolStripMenuItem helpToolStripMenuItem;

		internal CheckBox chkShowMeasurementMeans;

		private Button cmdSetLevelsAutomatic;

		private Panel panelTab5;

		private Label label5;

		private Label label50;

		private Label label51;

		private Label label52;

		private Label label53;

		private Label lbl_UT_R;

		private Label lbl_UT_D;

		private Label lbl_UTuncert_R;

		private Label lbl_UTuncert_D;

		internal Button cmdChangeIntegration;

		private Label label56;

		private Label label58;

		private Label label55;

		internal ComboBox cmbFrames;

		internal Button cmdSetTimeScale;

		private Label lblTimeSource;

		private Label label42;

		private Label label7;

		internal RadioButton optSystemPAL;

		internal RadioButton optSystemNTSC;

		private Panel panel2;

		private Panel panel1;

		internal GroupBox grpCameraCorrections;

		private Label lblTimeDelaySecs;

		private Label lblCameraDelayFrames;

		private Label lblIntegrationFrameDelay;

		private Label lblDFrameFinal;

		private Label lblRFrameFinal;

		private Label label6;

		private Label label45;

		private Label label44;

		private Label label43;

		private Label lblRtransition;

		private Label lblDtransition;

		private Label label46;

		internal Label lblEventIsMiss;

		internal TabPage tabPage7;

		internal GroupBox grpFinalResults;

		internal Button cmdSaveResults;

		internal Button cmdSetMiss;

		internal Button cmdSaveMissAnalysis;

		internal Button cmdDisplayReport;

		internal Button cmdSaveReport;

		internal RadioButton optSystemOther;

		internal Label lblSetNTSCorPAL;

		private ToolStripMenuItem exitToolStripMenuItem1;

		private ToolStripMenuItem withResultsToolStripMenuItem;

		private ToolStripMenuItem cancelNoResultsToolStripMenuItem;

		private GroupBox grpSaveImages;

		private Panel panelView;

		private ToolStripMenuItem hintsAndTipsToolStripMenuItem;

		internal Label lblTangraCameraCorrns;

		private ToolTip toolTip;

		internal Label lblEventsToAnalyse;

		private Label label48;

		private Button cmdSetAllToMiss;

		internal TabPage tabPage2;

		private Panel panelTab6;

		internal PictureBox picFourier;

		private ToolStripMenuItem saveFourierToolStripMenuItem;

		private ToolStripMenuItem copyFourierTransformOfLightCurveToolStripMenuItem;

		internal ComboBox cmbCamera;

		internal TabPage tabPage1;

		internal TabPage tabPage5;

		private Label label49;

		private Label label54;

		internal RadioButton optMonteCarloMeasuredSignal;

		internal RadioButton optMonteCarloTestSignal;

		internal RadioButton optChi2StdDevn;

		internal RadioButton optChi2Variance;

		private Panel panel3;

		private Panel panel4;

		internal ComboBox cmbPlotAverage;

		private Label label57;

		internal Label lblRunningInTangra;

		internal Button cmdVerifyTimes;

		internal Button cmdIntegrityCheck;

		internal RadioButton optSystemNotKnown;

		private Label label59;

		private Panel panel5;

		internal Label label60;

		internal CheckBox chkShowErrorBars;

		private Label label61;

		internal NumericUpDown updnConfidenceD;

		private Panel panel6;

		private Label label62;

		private Label label63;

		internal NumericUpDown updnConfidenceR;

		private Label label64;

		private ToolStripMenuItem starDiameterAnalysisToolStripMenuItem;

		private ToolStripMenuItem openStarDiameterAnalyserToolStripMenuItem;

		private ToolStripMenuItem copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem;

		private ToolStripMenuItem toolTipToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private Button cmdCheckIntegration;

		internal Label lblSelectCamera;

		internal Label lblSetIntegn;

		internal CheckBox chkTarget;

		internal CheckBox chkStarPoints;

		private Label label65;

		private Panel panelExpectedMagDrop;

		private Label label66;

		internal NumericUpDown updnExpectedMagDrop;

		private ToolStripMenuItem lightCurveToolStripMenuItem;

		private ToolStripMenuItem pasteCSVDataSetToolStripMenuItem;

		internal Button cmdReportLightCurve;

		internal CheckBox chkScaleComparisons;

		internal Label lblSNR;

		private TabPage tabPage3;

		internal ListBox lstComp3;

		internal ListBox lstComp2;

		internal ListBox lstComp1;

		internal ListBox lstTarget;

		private Label label69;

		private Label lblTarget;

		internal Label lblComp1;

		internal Label lblComp2;

		internal Label lblComp3;

		internal Label lblHead1;

		internal Label lblHead2;

		internal Label lblHead3;

		internal Label lblNoCompStars;

		private Panel panel7;

		internal RadioButton opt33;

		internal RadioButton opt25;

		internal RadioButton opt17;

		internal RadioButton opt49;

		internal Label label47;

		private Panel panel8;

		private Label label67;

		private Label label70;

		private Label label68;

		private Label label71;

		internal PictureBox picComp1;

		internal PictureBox picTarget;

		internal PictureBox picComp3;

		internal PictureBox picComp2;

		internal RadioButton opt9;

		internal RadioButton opt57;

		internal RadioButton opt41;

		private Label label72;

		private Label label75;

		private Panel panel9;

		internal Label lblRatio12;

		private Label lblShortEventWarning;

		internal ToolStripMenuItem plotUsingLargePointsToolStripMenuItem;

		private Panel pnlScale;

		private Label label76;

		private Label label77;

		private Panel panel11;

		private Panel panel10;

		private Label label73;

		internal Label lblPossible;

		internal Label label74;

		internal Label lblProbable;

		internal Label label78;

		internal Label lblFrame;

		private Label lblCopied;

		internal RadioButton optComp1;

		internal RadioButton optComp2;

		internal RadioButton optComp3;

		private Label lblMonitorScale;

		private Panel panel12;

		private ToolStripMenuItem saveImageOfFormToolStripMenuItem;

		private ToolStripMenuItem setScalecurrently100ToMatchMonitorScaleToolStripMenuItem;

		private Label label79;

		private CheckBox chkAddNameToSaves;

		private TextBox txtNameToAdd;

		private Label label80;

		internal ToolStripMenuItem alwaysShowComparisonsAtFileOpenToolStripMenuItem;

		internal ToolStripMenuItem correctForMissingExposuresToolStripMenuItem;

		public PlotForm()
		{
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Expected O, but got Unknown
			//IL_0498: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_055b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0561: Expected O, but got Unknown
			//IL_064f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0655: Expected O, but got Unknown
			//IL_0723: Unknown result type (might be due to invalid IL or missing references)
			//IL_0729: Expected O, but got Unknown
			//IL_07fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0800: Expected O, but got Unknown
			//IL_08fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0903: Expected O, but got Unknown
			InitializeComponent();
			if (Utilities.AppPath == null)
			{
				Utilities.AppPath = Path.GetDirectoryName(new Uri(Assembly.GetExecutingAssembly().Location).LocalPath);
			}
			RunningInDevelopmentEnvironment = Utilities.RunningInDevelopmentEnvironment;
			if (Utilities.AppPath.Contains("C#"))
			{
				RunningInDevelopmentEnvironment = true;
				if (Directory.Exists("C:\\Occult 4"))
				{
					Utilities.AppPath = "C:\\Occult 4";
				}
				else if (Directory.Exists("C:\\Program Files\\Occult 4"))
				{
					Utilities.AppPath = "C:\\Program Files\\Occult 4";
				}
			}
			OldScaleValue = (double)updnScale.get_Value();
			((ListControl)cmbIntegration).set_SelectedIndex(0);
			((Control)picChiR).set_Width(((Control)picChiD).get_Width());
			((Control)picChiR).set_Height(((Control)picChiD).get_Height());
			((Control)picRUncert).set_Width(((Control)picDUncert).get_Width());
			((Control)picRUncert).set_Height(((Control)picDUncert).get_Height());
			updnMaxDurn.set_Value(20m);
			AOTAData.PlotFormBaseText = "AOTA v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			if (RunningInDevelopmentEnvironment)
			{
				AOTAData.PlotFormBaseText += " [Development]";
			}
			((Control)this).set_Text(AOTAData.PlotFormBaseText);
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\CameraDelays.dat") && Utilities.InternetIsAvailable())
			{
				Downloads.Download_CameraDelays(SupressMessages: true);
			}
			cmbCamera.get_Items().Clear();
			((Control)lblEventIsMiss).set_Left(((Control)grpFinalResults).get_Left() + ((Control)label6).get_Left() + 5);
			((Control)lblEventIsMiss).set_Top(((Control)label6).get_Top() + 5);
			Delays.Clear();
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\CameraDelays.dat"))
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\CameraDelays.dat");
				do
				{
					string text = streamReader.ReadLine();
					if (!(text.Substring(0, 1) == "#"))
					{
						string[] array = text.Split(new char[1] { ',' });
						CameraDelays = new CameraDelays();
						CameraDelays.Camera = array[0].Trim();
						CameraDelays.IntegrationDelay = double.Parse(array[1], CultureInfo.InvariantCulture);
						CameraDelays.NonIntegrationDelay = double.Parse(array[2], CultureInfo.InvariantCulture);
						if (array.Length > 3)
						{
							CameraDelays.MultipleIntegrationDelay = double.Parse(array[3], CultureInfo.InvariantCulture);
						}
						else
						{
							CameraDelays.MultipleIntegrationDelay = 0.0;
						}
						Delays.Add(CameraDelays);
						cmbCamera.get_Items().Add((object)array[0].Trim());
					}
				}
				while (!streamReader.EndOfStream);
			}
			else
			{
				CameraDelays = new CameraDelays();
				CameraDelays.Camera = "No cameras available";
				CameraDelays.IntegrationDelay = 0.0;
				CameraDelays.NonIntegrationDelay = 0.0;
				Delays.Add(CameraDelays);
				cmbCamera.get_Items().Add((object)"No cameras available");
			}
			IsLoading = true;
			((ListControl)cmbFrames).set_SelectedIndex(0);
			if ((Settings.Default.CameraIndex >= 0) & (Settings.Default.CameraIndex < cmbCamera.get_Items().get_Count()))
			{
				((ListControl)cmbCamera).set_SelectedIndex(Settings.Default.CameraIndex);
			}
			else
			{
				((ListControl)cmbCamera).set_SelectedIndex(0);
			}
			AOTAData.CameraSpecs.CameraType = cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString();
			((ListControl)cmbPlotAverage).set_SelectedIndex(0);
			IsLoading = false;
			SetDelayTimes();
			for (int i = 0; i < 5; i++)
			{
				chkDisplayEvent[i] = new CheckBox();
				((Control)chkDisplayEvent[i]).set_Left(-90);
				((Control)chkDisplayEvent[i]).set_Top(15 + 21 * i);
				((Control)chkDisplayEvent[i]).set_Text("");
				((Control)chkDisplayEvent[i]).set_Visible(true);
				chkDisplayEvent[i].set_Checked(i == 0);
				chkDisplayEvent[i].set_CheckAlign(ContentAlignment.MiddleRight);
				chkDisplayEvent[i].set_AutoCheck(true);
				((Control)panelEvents).get_Controls().Add((Control)(object)chkDisplayEvent[i]);
				chkDisplayEvent[i].add_CheckedChanged((EventHandler)Check_Array);
			}
			for (int j = 0; j < 5; j++)
			{
				radioAddEvent[j] = new RadioButton();
				((Control)radioAddEvent[j]).set_Left(24);
				((Control)radioAddEvent[j]).set_Top(15 + 21 * j);
				((Control)radioAddEvent[j]).set_Text("Event #" + (j + 1));
				((Control)radioAddEvent[j]).set_Visible(true);
				((Control)radioAddEvent[j]).set_Enabled(j == 0);
				radioAddEvent[j].set_Checked(j == 0);
				radioAddEvent[j].set_AutoCheck(true);
				((Control)panelEvents).get_Controls().Add((Control)(object)radioAddEvent[j]);
				((Control)radioAddEvent[j]).BringToFront();
				radioAddEvent[j].add_CheckedChanged((EventHandler)Radio_Array);
			}
			for (int k = 0; k < 5; k++)
			{
				txtPositions[k] = new TextBox();
				((Control)txtPositions[k]).set_Left(100);
				((Control)txtPositions[k]).set_Top(15 + 21 * k);
				((Control)txtPositions[k]).set_Width(40);
				((Control)txtPositions[k]).set_Text("");
				((Control)txtPositions[k]).set_Visible(true);
				((Control)txtPositions[k]).set_Enabled(k == 0);
				((TextBoxBase)txtPositions[k]).set_BorderStyle((BorderStyle)1);
				((TextBoxBase)txtPositions[k]).set_ReadOnly(true);
				((Control)panelEvents).get_Controls().Add((Control)(object)txtPositions[k]);
				((Control)txtPositions[k]).BringToFront();
			}
			for (int l = 0; l < 5; l++)
			{
				txtWidths[l] = new TextBox();
				((Control)txtWidths[l]).set_Left(160);
				((Control)txtWidths[l]).set_Top(15 + 21 * l);
				((Control)txtWidths[l]).set_Width(40);
				((Control)txtWidths[l]).set_Text("");
				((Control)txtWidths[l]).set_Visible(true);
				((Control)txtWidths[l]).set_Enabled(l == 0);
				((TextBoxBase)txtWidths[l]).set_BorderStyle((BorderStyle)1);
				((TextBoxBase)txtWidths[l]).set_ReadOnly(true);
				((Control)panelEvents).get_Controls().Add((Control)(object)txtWidths[l]);
				((Control)txtWidths[l]).BringToFront();
			}
			for (int m = 0; m < 5; m++)
			{
				cmdEvaluate[m] = new Button();
				((Control)cmdEvaluate[m]).set_Left(230);
				((Control)cmdEvaluate[m]).set_Top(15 + 21 * m);
				((Control)cmdEvaluate[m]).set_Width(80);
				((Control)cmdEvaluate[m]).set_Text("Analyse #" + (m + 1));
				((Control)cmdEvaluate[m]).set_Font(new Font("Arial", 9f, FontStyle.Bold));
				((Control)cmdEvaluate[m]).set_Visible(true);
				((Control)cmdEvaluate[m]).set_Enabled(m == 0);
				((Control)panelEvents).get_Controls().Add((Control)(object)cmdEvaluate[m]);
				((Control)cmdEvaluate[m]).BringToFront();
				((Control)cmdEvaluate[m]).add_Click((EventHandler)Evaluate_Setup);
			}
			for (int n = 0; n < 5; n++)
			{
				chkSetAsMiss[n] = new CheckBox();
				((Control)chkSetAsMiss[n]).set_Left(325);
				((Control)chkSetAsMiss[n]).set_Top(15 + 21 * n);
				((Control)chkSetAsMiss[n]).set_Text("");
				((Control)chkSetAsMiss[n]).set_Visible(true);
				chkSetAsMiss[n].set_Checked(n > 0);
				chkSetAsMiss[n].set_CheckAlign(ContentAlignment.MiddleCenter);
				chkSetAsMiss[n].set_AutoCheck(true);
				((Control)chkSetAsMiss[n]).set_Tag((object)n);
				((Control)panelEvents).get_Controls().Add((Control)(object)chkSetAsMiss[n]);
				chkSetAsMiss[n].add_CheckedChanged((EventHandler)SetAsMiss);
			}
			try
			{
				SetSizes(ResizeEvent: false);
			}
			catch
			{
			}
			Chi2.CreateTransitionSignals();
			T.set_Interval(1200);
			T.add_Tick((EventHandler)delegate
			{
				((Control)lblShortEventWarning).Hide();
				T.Stop();
			});
			Tcopy.set_Interval(600);
			Tcopy.add_Tick((EventHandler)delegate
			{
				((Control)lblCopied).Hide();
				Tcopy.Stop();
			});
		}

		private void PlotForm_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			Panel obj = panelTab1;
			Panel obj2 = panelTab2;
			Panel obj3 = panelTab3;
			Panel obj4 = panelTab4;
			int width;
			((Control)panelTab5).set_Width(width = ((Control)tabPlot).get_Width());
			int num;
			((Control)obj4).set_Width(num = width);
			int num2;
			((Control)obj3).set_Width(num2 = num);
			int width2;
			((Control)obj2).set_Width(width2 = num2);
			((Control)obj).set_Width(width2);
			Panel obj5 = panelTab1;
			Panel obj6 = panelTab2;
			Panel obj7 = panelTab3;
			Panel obj8 = panelTab4;
			((Control)panelTab5).set_Left(width = 0);
			((Control)obj8).set_Left(num = width);
			((Control)obj7).set_Left(num2 = num);
			((Control)obj6).set_Left(width2 = num2);
			((Control)obj5).set_Left(width2);
			try
			{
				((ListControl)cmbCamera).set_SelectedIndex(Settings.Default.CameraIndex);
			}
			catch
			{
			}
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Text("Set scale (currently " + Settings.Default.MonitorScale + "%) to match Monitor scale");
			((Control)lblMonitorScale).set_Text("Monitor scale: " + Settings.Default.MonitorScale + "%");
			alwaysShowComparisonsAtFileOpenToolStripMenuItem.set_Checked(Settings.Default.AOTAshowComparisons);
			ToolStripMenuItem obj10 = toolTipToolStripMenuItem;
			bool showToolTip;
			toolTip.set_Active(showToolTip = ShowToolTip);
			obj10.set_Checked(showToolTip);
			AOTAData.MaximimumTransitions = (int)updnTransitionNumber.get_Value();
			Chi2.InitialiseVariables();
			Chi2.CreateTransitionSignals();
			SetRanges();
			Chi2.MonteCarloTrials = (int)updnMonteCarlo.get_Value();
			Gaussian.MultipleOfSD = (double)updnSDLimit.get_Value();
			optMonteCarloTestSignal_CheckedChanged(sender, e);
			optChi2Variance_CheckedChanged(sender, e);
			AOTAData.ConfidenceLevelD = (double)updnConfidenceD.get_Value() / 100.0;
			AOTAData.ConfidenceLevelR = (double)updnConfidenceR.get_Value() / 100.0;
			SetMaxFrames();
			((Control)AOTAData.PlotForm.lblRunningInTangra).set_Visible(AOTA_ExternalAccess.AOTA_Client != null);
			AOTA_ExternalAccess.TangraFrameDisplayFromTangra = (AOTA_ExternalAccess.TangraFrameDisplayFromAOTA = false);
			if (AOTA_ExternalAccess.RunFromTangra)
			{
				AOTAData.StarLevel /= AOTAData.StarCount;
				AOTAData.Comp1Level /= AOTAData.StarCount;
				AOTAData.Comp2Level /= AOTAData.StarCount;
				AOTAData.Comp3Level /= AOTAData.StarCount;
				AOTAData.Background /= AOTAData.StarCount;
				((Control)chkComp1).set_Visible(AOTAData.Comp1Used);
				((Control)chkComp2).set_Visible(AOTAData.Comp2Used);
				((Control)chkComp3).set_Visible(AOTAData.Comp3Used);
				((Control)chkShowBackground).set_Visible(AOTAData.Background != 0f);
				chkStarPoints.set_Checked(true);
				chkComp1.set_Checked(false);
				chkComp2.set_Checked(false);
				chkComp3.set_Checked(false);
				chkShowBackground.set_Checked(false);
				((ListControl)cmbPlotAverage).set_SelectedIndex(0);
				if (AOTAData.IsPAL)
				{
					optPAL.set_Checked(true);
				}
				else
				{
					optNTSC.set_Checked(true);
				}
				CameraDelaySecs = 0.0;
				updnVerticalScaleAdjustment.set_Value(1m);
				((Control)this).set_Text(AOTAData.PlotFormBaseText + ("    File being processed is " + AOTAData.TangraSourceFile + ", measured with Tangra").Replace("s , m", "s m"));
				((ListControl)cmbFrames).set_SelectedIndex(1);
				((Control)cmdChangeIntegration).set_Enabled(true);
				AOTAData.NormalisationStarRef = 0;
				tabPlot.set_SelectedIndex(3);
				((Control)grpCameraCorrections).set_Enabled(!AOTAData.CameraCorrectionsHaveBeenApplied);
				((Control)lblTangraCameraCorrns).set_Visible(AOTAData.CameraCorrectionsHaveBeenApplied);
				string[] array = new string[3] { "ADVS", "AAV", "SER" };
				for (int i = 0; i < array.Length; i++)
				{
					if (!AOTAData.CameraSpecs.VideoSystem.Contains(array[i]))
					{
						continue;
					}
					optSystemOther.set_Checked(true);
					for (int j = 0; j < AOTAData.PlotForm.cmbCamera.get_Items().get_Count(); j++)
					{
						if (AOTAData.PlotForm.cmbCamera.get_Items().get_Item(j).ToString()!.Contains(array[i]))
						{
							Settings @default = Settings.Default;
							((ListControl)AOTAData.PlotForm.cmbCamera).set_SelectedIndex(width2 = j);
							@default.CameraIndex = width2;
							break;
						}
					}
					break;
				}
				AOTAData.ResetOutputs(JustOne: false, -1);
				if (AOTAData.CameraSpecs.CameraType == "")
				{
					AOTAData.CameraSpecs.CameraType = AOTAData.Camera_from_Tangra;
				}
				AOTAData.CameraSpecs.TimeScaleFromMeasuringTool = AOTAData.TimePresentInSourceData;
				AOTAData.CameraSpecs.MeasuringTool = "Tangra";
				AOTAData.CameraSpecs.MeasuredAtFieldLevel = AOTAData.FrameID[10] - AOTAData.FrameID[0] < 8f;
				if (AOTAData.CameraSpecs.VideoSystem == "")
				{
					AOTAData.CameraSpecs.VideoSystem = "Unspecified, or ADVS";
				}
				AOTAData.CameraSpecs.MeasurementsBinned = 1;
				AOTAData.CameraSpecs.FramesIntegrated = AOTAData.FramesInIntegration_fromTangra;
				AOTAData.CameraSpecs.CameraDelaysKnownToAOTA = true;
				for (int k = 0; k < cmbFrames.get_Items().get_Count(); k++)
				{
					if (int.Parse(cmbFrames.get_Items().get_Item(k).ToString()) <= AOTAData.FramesInIntegration_fromTangra)
					{
						((ListControl)cmbFrames).set_SelectedIndex(k);
						break;
					}
				}
				Label obj11 = lblSetIntegn;
				((Control)lblSelectCamera).set_Visible(showToolTip = false);
				((Control)obj11).set_Visible(showToolTip);
				if (AOTAData.CameraCorrectionsHaveBeenApplied)
				{
					optSystemOther.set_Checked(true);
					((Control)lblSetNTSCorPAL).set_Visible(false);
					AOTAData.CameraSpecs.CameraDelaysKnownToAOTA = false;
				}
				else if (AOTAData.CameraSpecs.VideoSystem.Contains("PAL"))
				{
					AOTAData.IsPAL = true;
					optPAL.set_Checked(true);
					optSystemPAL.set_Checked(true);
					((Control)lblSetNTSCorPAL).set_Visible(false);
				}
				else if (AOTAData.CameraSpecs.VideoSystem.Contains("NTSC"))
				{
					AOTAData.IsPAL = false;
					optNTSC.set_Checked(true);
					optSystemNTSC.set_Checked(true);
					((Control)lblSetNTSCorPAL).set_Visible(false);
				}
				else if (AOTAData.CameraSpecs.VideoSystem.Contains("AAV") & AOTAData.SetCameraDelays_AAV_Files)
				{
					optSystemOther.set_Checked(true);
					((Control)lblSetNTSCorPAL).set_Visible(false);
					Label obj12 = lblSetIntegn;
					((Control)lblSelectCamera).set_Visible(showToolTip = false);
					((Control)obj12).set_Visible(showToolTip);
				}
				else
				{
					optSystemOther.set_Checked(true);
					((Control)lblSetNTSCorPAL).set_Visible(true);
				}
				((ToolStripItem)exitToolStripMenuItem).set_Visible(false);
				((ToolStripItem)exitToolStripMenuItem1).set_Visible(true);
				((ToolStripItem)openCSVFileToolStripMenuItem).set_Enabled(false);
				AOTAData.AutoProcess = true;
				if (!AOTAData.TimePresentInSourceData)
				{
					AOTAData.SetTimeScale();
				}
				((Control)AOTAData.PlotForm).Focus();
				AOTAData.PlotForm.tabPlot.set_SelectedIndex(2);
				if (!AOTAData.TimePresentInSourceData | AOTAData.OnlySomeTimesPresentInSourceData)
				{
					AOTAData.SetTimeScale();
				}
				AOTAData.ShowIntegrityCheck(FromLightCurve: false);
			}
			else
			{
				((ToolStripItem)exitToolStripMenuItem).set_Visible(true);
				((ToolStripItem)exitToolStripMenuItem1).set_Visible(false);
				AOTA_ExternalAccess.AOTA_Client = null;
			}
		}

		private void Radio_Array(object sender, EventArgs e)
		{
			IndexOfEventToUpdate = 0;
			for (int i = 0; i < 5; i++)
			{
				if (radioAddEvent[i].get_Checked())
				{
					IndexOfEventToUpdate = i;
				}
			}
			AOTAData.PlotAOTA();
		}

		private void Check_Array(object sender, EventArgs e)
		{
			for (int i = 0; i < 5; i++)
			{
				RadioButton obj = radioAddEvent[i];
				bool @checked;
				((Control)cmdEvaluate[i]).set_Enabled(@checked = chkDisplayEvent[i].get_Checked());
				((Control)obj).set_Enabled(@checked);
				if (radioAddEvent[i].get_Checked() & !((Control)radioAddEvent[i]).get_Enabled())
				{
					radioAddEvent[i].set_Checked(false);
				}
			}
			AOTAData.PlotAOTA();
		}

		private void Evaluate_Setup(object sender, EventArgs e)
		{
			if (AOTAData.MaxValues.Count < 1)
			{
				return;
			}
			SetMissArray();
			for (int i = 0; i < 5; i++)
			{
				if (!sender.ToString()!.Contains((i + 1).ToString()))
				{
					continue;
				}
				AOTAData.EventAnalysed[i] = true;
				if ((AOTAData.MaxValues[i].Width < AOTAData.ExtraPointsUsedInSolution) | ((AOTAData.ExtraPointsUsedInSolution < 10) & (AOTAData.MaxValues[i].Width > AOTAData.ExtraPointsUsedInSolution)))
				{
					AOTAData.UpdatingExtraPointsUsedInSolution = true;
					AOTAData.ExtraPointsUsedInSolution = AOTAData.MaxValues[i].Width;
					if (AOTAData.ExtraPointsUsedInSolution > 10)
					{
						AOTAData.ExtraPointsUsedInSolution = 10;
					}
					if (AOTAData.ExtraPointsUsedInSolution < 2)
					{
						AOTAData.ExtraPointsUsedInSolution = 2;
					}
					updnExtraPoints.set_Value((decimal)AOTAData.ExtraPointsUsedInSolution);
					AOTAData.UpdatingExtraPointsUsedInSolution = false;
				}
				chkSetAsMiss[i].set_Checked(false);
				AOTAData.EvaluateSelectedEvent(i);
				break;
			}
		}

		private void SetAsMiss(object sender, EventArgs e)
		{
			SetMissArray();
			for (int i = 0; i < 5; i++)
			{
				if (i == AOTAData.CurrentSolution)
				{
					((Control)lblEventIsMiss).set_Visible(AOTAData.MissEvent[AOTAData.CurrentSolution]);
					if (AOTAData.MissEvent[AOTAData.CurrentSolution])
					{
						((Control)lblEventIsMiss).set_Text("Event #" + AOTAData.EventID + " is set\r\nas a Non-Event\r\n(see tab 4)");
					}
				}
			}
		}

		internal void SetMissArray()
		{
			for (int i = 0; i < 5; i++)
			{
				AOTAData.MissEvent[i] = chkSetAsMiss[i].get_Checked();
				AOTAData.Results[i].IsNonEvent = chkSetAsMiss[i].get_Checked();
			}
		}

		private void PlotForm_Resize(object sender, EventArgs e)
		{
			SetSizes(ResizeEvent: true);
		}

		internal void SetSizes(bool ResizeEvent)
		{
			//IL_0583: Unknown result type (might be due to invalid IL or missing references)
			//IL_0589: Invalid comparison between Unknown and I4
			int num = 200;
			int num2 = 0;
			num = (int)((decimal)AOTAData.StarCountToPlot * updnScale.get_Value());
			if (num < 200)
			{
				num = 200;
			}
			((Control)panelPlot).set_Width(((Control)this).get_Width() - 25);
			((Control)panelPlot).set_Height(((Control)this).get_Height() - 280);
			((Control)picPlot).set_Width(num);
			((Control)picPlot).set_Height(((Control)panelPlot).get_Height() - 17);
			((Control)tabPlot).set_Top(((Control)this).get_Height() - 232);
			((Control)pnlScale).set_Top(((Control)this).get_Height() - 232);
			((Control)panelComp).set_Top(((Control)pnlScale).get_Top() + ((Control)pnlScale).get_Height());
			int num3 = (int)((double)((float)((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Value() + (float)((Control)this).get_Width() / 2f) / OldScaleValue);
			((Control)tabPlot).set_Width(((Control)this).get_Width() - 140);
			Panel obj = panelEvents;
			Panel obj2 = panelTab1;
			Panel obj3 = panelTab2;
			Panel obj4 = panelTab3;
			Panel obj5 = panelTab4;
			Panel obj6 = panelTab5;
			int width;
			((Control)panelTab6).set_Width(width = ((Control)tabPlot).get_Width());
			int num4;
			((Control)obj6).set_Width(num4 = width);
			int num5;
			((Control)obj5).set_Width(num5 = num4);
			int num6;
			((Control)obj4).set_Width(num6 = num5);
			int num7;
			((Control)obj3).set_Width(num7 = num6);
			int width2;
			((Control)obj2).set_Width(width2 = num7);
			((Control)obj).set_Width(width2);
			((Control)panelEvents).set_Left(45);
			((Control)panel8).set_Left(((Control)tabPage3).get_Width() - ((Control)panel8).get_Width() - 5);
			if (((Control)tabPlot).get_Width() > 900)
			{
				num2 = ((Control)tabPlot).get_Width() - 900;
				((Control)grpExpected).set_Left(268 + (int)(0.298 * (double)num2));
				((Control)grpMaxDurn).set_Left(434 + (int)(0.482 * (double)num2));
				((Control)panelFind).set_Left(579 + (int)(0.643 * (double)num2));
				for (int i = 0; i < 5; i++)
				{
					((Control)chkDisplayEvent[i]).set_Left(-90);
					((Control)radioAddEvent[i]).set_Left(24 + (int)(0.08 * (double)num2));
					((Control)txtPositions[i]).set_Left(100 + (int)(0.14 * (double)num2));
					((Control)txtWidths[i]).set_Left(160 + (int)(0.15 * (double)num2));
					((Control)cmdEvaluate[i]).set_Left(230 + (int)(0.18 * (double)num2));
					((Control)chkSetAsMiss[i]).set_Left(325 + (int)(0.25 * (double)num2));
				}
				((Control)label16).set_Left(-3);
				((Control)label17).set_Left(((Control)radioAddEvent[0]).get_Left());
				((Control)label18).set_Left(((Control)txtPositions[0]).get_Left());
				((Control)label19).set_Left(((Control)txtWidths[0]).get_Left());
				((Control)label46).set_Left(((Control)chkSetAsMiss[0]).get_Left() + 5);
				((Control)cmdSetAllToMiss).set_Left(((Control)chkSetAsMiss[0]).get_Right());
				((Control)panelExpectedMagDrop).set_Left(((Control)cmdSetAllToMiss).get_Right() + 30);
				Panel obj7 = panelChi2;
				((Control)panelMonteCarlo).set_Left(width2 = 148 + (int)(0.164 * (double)num2));
				((Control)obj7).set_Left(width2);
				((Control)panelGetLocations).set_Left(307 + (int)(0.341 * (double)num2));
				((Control)panelTransitions).set_Left(499 + (int)(0.554 * (double)num2));
				((Control)panelDR).set_Left(703 + (int)(0.781 * (double)num2));
				((Control)panelUncertainty).set_Left(723 + (int)(0.803 * (double)num2));
				PictureBox obj8 = picChiD;
				((Control)picChiR).set_Width(width2 = 193 + (int)(0.225 * (double)num2));
				((Control)obj8).set_Width(width2);
				((Control)panelTransitions).set_Width(((Control)picChiD).get_Width() + 5);
				PictureBox obj9 = picDUncert;
				((Control)picRUncert).set_Width(width2 = 156 + (int)(0.182 * (double)num2));
				((Control)obj9).set_Width(width2);
				((Control)panelUncertainty).set_Width(((Control)picRUncert).get_Width() + 9);
				((Control)grpFinalResults).set_Left(359 + (int)(0.399 * (double)num2));
				((Control)grpSaveImages).set_Left(587 + (int)(0.652 * (double)num2));
				((Control)panelView).set_Left(720 + (int)(0.8 * (double)num2));
				((Control)lblEventIsMiss).set_Left(((Control)grpFinalResults).get_Left() + ((Control)label6).get_Left() + 5);
				((Control)picFourier).set_Width(((Control)panelTab6).get_Width());
				((Control)picFourier).set_Left(0);
				AOTAData.PlotFourier();
			}
			if (ResizeEvent)
			{
				if (this == null)
				{
					return;
				}
				if ((int)((Form)this).get_WindowState() != 1)
				{
					AOTAData.PlotAOTA();
					AOTAData.PlotChi2();
					AOTAData.PlotUncertainties();
				}
			}
			OldScaleValue = (double)updnScale.get_Value();
			int num8 = (int)((double)num3 * OldScaleValue - (double)((float)((Control)this).get_Width() / 2f) + OldScaleValue / 2.0);
			if (num8 < 1)
			{
				num8 = 1;
			}
			if (num8 > 32000)
			{
				num8 = 31000;
			}
			if (num8 > ((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Maximum())
			{
				num8 = ((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Maximum();
			}
			try
			{
				((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(num8);
			}
			catch
			{
			}
		}

		private void cmdOpen_Click(object sender, EventArgs e)
		{
			OpenNewFile(correctForMissingExposuresToolStripMenuItem.get_Checked());
		}

		private void openCSVFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			OpenNewFile(correctForMissingExposuresToolStripMenuItem.get_Checked());
		}

		private void OpenNewFile(bool AddMissingFrames)
		{
			IsLoading = true;
			RadioButton obj = optComp1;
			RadioButton obj2 = optComp2;
			bool flag;
			optComp3.set_Checked(flag = false);
			bool @checked;
			obj2.set_Checked(@checked = flag);
			obj.set_Checked(@checked);
			AOTAData.OpenCSVfile(FromLightCurve: false, "", AddMissingFrames);
			IsLoading = false;
			SetSizes(ResizeEvent: false);
			SetMaxFrames();
			((Control)cmdChangeIntegration).set_Enabled(true);
			AOTAData.PlotAOTA();
		}

		private void pasteCSVDataSetToolStripMenuItem_Click(object sender, EventArgs e)
		{
			IsLoading = true;
			RadioButton obj = optComp1;
			RadioButton obj2 = optComp2;
			bool flag;
			optComp3.set_Checked(flag = false);
			bool @checked;
			obj2.set_Checked(@checked = flag);
			obj.set_Checked(@checked);
			AOTAData.PasteCSVdata(FromLightCurve: false, correctForMissingExposuresToolStripMenuItem.get_Checked());
			IsLoading = false;
			SetSizes(ResizeEvent: false);
			SetMaxFrames();
			((Control)cmdChangeIntegration).set_Enabled(true);
			AOTAData.PlotAOTA();
		}

		private void updnScale_ValueChanged(object sender, EventArgs e)
		{
			SetSizes(ResizeEvent: false);
			AOTAData.PlotAOTA();
			if (updnScale.get_Value() >= 5m)
			{
				updnScale.set_DecimalPlaces(1);
				updnScale.set_Increment(1m);
			}
			else if (updnScale.get_Value() >= 2m)
			{
				updnScale.set_DecimalPlaces(1);
				updnScale.set_Increment(0.5m);
			}
			else if (updnScale.get_Value() >= 0.8m)
			{
				updnScale.set_DecimalPlaces(1);
				updnScale.set_Increment(0.1m);
			}
			else
			{
				updnScale.set_DecimalPlaces(2);
				updnScale.set_Increment(0.05m);
			}
		}

		private void panelPlot_Scroll(object sender, ScrollEventArgs e)
		{
			try
			{
				Application.DoEvents();
			}
			catch
			{
			}
		}

		private void chkStarPoints_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotAOTA();
			}
		}

		private void chkTarget_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotAOTA();
			}
		}

		private void chkComp1_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotAOTA();
			}
		}

		private void chkComp3_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotAOTA();
			}
		}

		private void chkShowBackground_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotAOTA();
			}
		}

		private void picPlot_MouseMove(object sender, MouseEventArgs e)
		{
			if (AOTAData.StarCount >= 1 && !AOTAData.IsPlotting)
			{
				float num = e.get_X();
				float num2 = e.get_Y();
				int num3 = (int)((double)num / OldScaleValue + 0.5);
				int num4 = AOTAData.FirstFrameUsedInAnalysis + AOTAData.NumberOfFramesBinned * num3;
				if (num4 < 0)
				{
					num4 = 0;
				}
				if (num4 >= AOTAData.StarCount)
				{
					num4 = AOTAData.StarCount - 1;
				}
				float num6;
				if (AOTAData.AOTA_Height_100 > 5f)
				{
					float num5 = ((float)((Control)picPlot).get_Height() - AOTAData.AOTA_ZeroHeight - num2) / AOTAData.AOTA_VerticalScale;
					num6 = 100f * num5 / AOTAData.AOTA_Height_100;
				}
				else
				{
					float num7 = (float)((Control)picPlot).get_Height() - AOTAData.AOTA_ZeroHeight;
					num6 = 100f * (num7 - num2) / num7;
				}
				string text = string.Format(" | Height: {0,1:f0}", num6);
				if (AOTAData.AOTA_Height_100 > 5f)
				{
					text += "%";
				}
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip.SetToolTip((Control)(object)picPlot, AOTAData.FrameID[num4] + text + " | " + Utilities.DEGtoDMS((AOTAData.FrameTimeSecs[num4] - CameraDelaySecs) / 3600.0, 2, 2, MinutesOnly: false));
				}
			}
		}

		private void picPlot_MouseClick(object sender, MouseEventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0015: Invalid comparison between Unknown and I4
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Invalid comparison between Unknown and I4
			//IL_0055: Unknown result type (might be due to invalid IL or missing references)
			//IL_005f: Invalid comparison between Unknown and I4
			float num = e.get_X();
			int num2 = 0;
			if ((int)e.get_Button() == 2097152)
			{
				if ((int)Control.get_ModifierKeys() == 131072)
				{
					int num3 = (int)((double)num / OldScaleValue + 0.5);
					AOTAData.StarValidToPlot[num3] = !AOTAData.StarValidToPlot[num3];
					AnalyseForDandR(0, 0);
				}
				else if ((int)Control.get_ModifierKeys() == 262144)
				{
					float num4 = e.get_Y();
					if (num4 < 5f)
					{
						AOTAData.AOTA_Height_100 = 0f;
					}
					else
					{
						AOTAData.AOTA_Height_100 = ((float)((Control)picPlot).get_Height() - AOTAData.AOTA_ZeroHeight - num4) / AOTAData.AOTA_VerticalScale;
					}
					AOTAData.PlotAOTA();
				}
				else if (AOTA_ExternalAccess.AOTA_Client != null)
				{
					AOTA_ExternalAccess.FrameID_DisplayedInTangra = AOTAData.FrameIDofPlot[(int)((double)num / OldScaleValue + 0.5)];
					AOTA_ExternalAccess.AOTA_Client.PositionToFrame((int)AOTA_ExternalAccess.FrameID_DisplayedInTangra);
					AOTA_ExternalAccess.TangraFrameDisplayFromAOTA = true;
					AOTA_ExternalAccess.TangraFrameDisplayFromTangra = false;
					AOTAData.PlotAOTA();
				}
				return;
			}
			if (tabPlot.get_SelectedIndex() == 3)
			{
				if (optFullLight.get_Checked())
				{
					if (AOTAData.StarCountToPlot < 10)
					{
						return;
					}
					AOTAData.XFull = (int)((double)num / OldScaleValue);
					((Control)txtFullPosn).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[AOTAData.XFull]));
					AOTAData.GetMean_SD(AOTAData.XFull - 20, AOTAData.XFull + 20, out AOTAData.YFull, out AOTAData.YFullSD, out AOTAData.YFullVariance);
					((Control)txtFullLightLevel).set_Text(string.Format("{0,1:f1}", AOTAData.YFull));
					MagDropCalculator(UpdateMagDrop: true);
					optPossOccLevel.set_Checked(true);
				}
				else if (optPossOccLevel.get_Checked())
				{
					AOTAData.XOcc = (int)((double)num / OldScaleValue);
					((Control)txtOccLevelPos).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[AOTAData.XOcc]));
					AOTAData.GetMean_SD(AOTAData.XOcc - 2, AOTAData.XOcc + 2, out AOTAData.YOcc, out AOTAData.YOccSD, out AOTAData.YOccVariance);
					((Control)txtOccultedLightLevel).set_Text(string.Format("{0,1:f1}", AOTAData.YOcc));
				}
				if (Math.Abs(AOTAData.YFullSD + AOTAData.YOccSD) < 0.1)
				{
					((Control)txtExpectedSN).set_Text("0");
				}
				else
				{
					((Control)txtExpectedSN).set_Text(string.Format("{0,1:f1}", 2.0 * (AOTAData.YFull - AOTAData.YOcc) / (AOTAData.YFullSD + AOTAData.YOccSD)));
				}
			}
			else if (tabPlot.get_SelectedIndex() == 4)
			{
				if (IndexOfEventToUpdate == 0 || AOTAData.MaxValues.Count < 1)
				{
					return;
				}
				AOTAData.MaxValues[IndexOfEventToUpdate].FOM = 0.0;
				num2 = (int)((double)num / OldScaleValue);
				for (int i = num2 - 20; i < num2 + 20; i++)
				{
					if (!((num2 < 0) | (num2 > AOTAData.StarCountToPlot - 1)) && AOTAData.CrossCorrR[i] > AOTAData.MaxValues[IndexOfEventToUpdate].FOM)
					{
						AOTAData.MaxValues[IndexOfEventToUpdate].FOM = AOTAData.CrossCorrR[num2];
						AOTAData.MaxValues[IndexOfEventToUpdate].Pos = num2;
						AOTAData.MaxValues[IndexOfEventToUpdate].Width = AOTAData.CrossCorrWidth[num2];
					}
				}
				((Control)txtPositions[IndexOfEventToUpdate]).set_Text(AOTAData.MaxValues[IndexOfEventToUpdate].Pos.ToString());
				((Control)txtWidths[IndexOfEventToUpdate]).set_Text(AOTAData.MaxValues[IndexOfEventToUpdate].Width.ToString());
			}
			else if (tabPlot.get_SelectedIndex() == 5)
			{
				if (optAfterR.get_Checked())
				{
					AOTAData.X6 = (int)((double)num / OldScaleValue);
					((Control)txtAfterR).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[AOTAData.X6]));
					AOTAData.X7 = AOTAData.X6 + AOTAData.ExtraPointsUsedInSolution;
					if (AOTAData.X7 >= AOTAData.StarCount)
					{
						AOTAData.X7 = AOTAData.StarCount - 1;
					}
				}
				else if (optBeforeR.get_Checked())
				{
					AOTAData.X5 = (int)((double)num / OldScaleValue);
					((Control)txtBeforeR).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[AOTAData.X5]));
					optAfterR.set_Checked(true);
					AOTAData.X4 = AOTAData.X5 - AOTAData.ExtraPointsUsedInSolution;
				}
				else if (optAfterD.get_Checked())
				{
					AOTAData.X2 = (int)((double)num / OldScaleValue);
					((Control)txtAfterD).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[AOTAData.X2]));
					optBeforeR.set_Checked(true);
					AOTAData.X3 = AOTAData.X2 + AOTAData.ExtraPointsUsedInSolution;
				}
				else if (optBeforeD.get_Checked())
				{
					AOTAData.X1 = (int)((double)num / OldScaleValue);
					((Control)txtBeforeD).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[AOTAData.X1]));
					AOTAData.X0 = AOTAData.X1 - AOTAData.ExtraPointsUsedInSolution;
					if (AOTAData.X0 < 1)
					{
						AOTAData.X0 = 1;
					}
					optAfterD.set_Checked(true);
				}
			}
			AOTAData.PlotAOTA();
		}

		private void cmdChangeIntegration_Click(object sender, EventArgs e)
		{
			AOTAData.Set_DataToPlot(FromLightCurve: false);
		}

		private void cmdSetTimeScale_Click(object sender, EventArgs e)
		{
			AOTAData.SetTimeScale();
		}

		private void updnMaxDurn_ValueChanged(object sender, EventArgs e)
		{
			SetMaxFrames();
		}

		private void optNTSC_CheckedChanged(object sender, EventArgs e)
		{
			SetMaxFrames();
		}

		private void optPAL_CheckedChanged(object sender, EventArgs e)
		{
			SetMaxFrames();
		}

		private void cmbIntegration_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetMaxFrames();
		}

		private void SetMaxFrames()
		{
			int num = 30;
			if (optPAL.get_Checked())
			{
				num = 25;
			}
			int num2 = Convert.ToInt32(cmbIntegration.get_Items().get_Item(((ListControl)cmbIntegration).get_SelectedIndex()).ToString());
			try
			{
				AOTAData.MaxFrames = (int)((double)((int)updnMaxDurn.get_Value() * num / num2) * 1.4);
				((Control)txtMaxFrames).set_Text(AOTAData.MaxFrames.ToString());
			}
			catch
			{
			}
		}

		private void cmdFindPossible_Click(object sender, EventArgs e)
		{
			FindPossibleEvents();
		}

		internal void FindPossibleEvents()
		{
			//IL_003a: Unknown result type (might be due to invalid IL or missing references)
			if (AOTAData.StarCountToPlot < 10)
			{
				return;
			}
			if ((AOTAData.YFull == 0.0) | (AOTAData.YOcc == 0.0))
			{
				MessageBox.Show("You must set the levels before\r\nyou can search for possible events", "Levels not set", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			AOTAData.FindPossibleOccultations(AOTAData.YFull, AOTAData.YOcc, AOTAData.MaxFrames);
			tabPlot.set_SelectedIndex(4);
			if (AOTAData.AutoProcess)
			{
				SetMissArray();
				if ((AOTAData.MaxValues[0].Width < AOTAData.ExtraPointsUsedInSolution) | ((AOTAData.ExtraPointsUsedInSolution < 10) & (AOTAData.MaxValues[0].Width > AOTAData.ExtraPointsUsedInSolution)))
				{
					AOTAData.UpdatingExtraPointsUsedInSolution = true;
					AOTAData.ExtraPointsUsedInSolution = AOTAData.MaxValues[0].Width;
					if (AOTAData.ExtraPointsUsedInSolution > 10)
					{
						AOTAData.ExtraPointsUsedInSolution = 10;
					}
					if (AOTAData.ExtraPointsUsedInSolution < 2)
					{
						AOTAData.ExtraPointsUsedInSolution = 2;
					}
					updnExtraPoints.set_Value((decimal)AOTAData.ExtraPointsUsedInSolution);
					AOTAData.UpdatingExtraPointsUsedInSolution = false;
				}
				AOTAData.EventAnalysed[0] = true;
				AOTAData.EvaluateSelectedEvent(0);
				tabPlot.set_SelectedIndex(5);
			}
			AOTAData.IntegrationPreset = false;
		}

		private void tabPlot_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (tabPlot.get_SelectedIndex() == 2)
			{
				((Control)lblShortEventWarning).Show();
				T.Start();
			}
			if (tabPlot.get_SelectedIndex() == 3)
			{
				optFullLight.set_Checked(true);
			}
			if (tabPlot.get_SelectedIndex() == 4)
			{
				optBeforeD.set_Checked(true);
			}
			((ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem).set_Visible(tabPlot.get_SelectedIndex() > 4);
			AOTAData.PlotAOTA();
			if (tabPlot.get_SelectedIndex() == 6)
			{
				CheckIntegrationMatchesBinning();
				((Control)grpCameraCorrections).set_Enabled(true);
				SetCameraBackgroundForAAV();
			}
		}

		private void SetCameraBackgroundForAAV()
		{
			int selectedIndex = ((ListControl)cmbCamera).get_SelectedIndex();
			if ((AOTAData.IsAAVFile | AOTAData.IsADVSFile) & (cmbCamera.get_Items().get_Item(selectedIndex).ToString()!.Contains("AAV") | cmbCamera.get_Items().get_Item(selectedIndex).ToString()!.Contains("ADVS")))
			{
				((Control)cmbCamera).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold));
				if (AOTAData.CameraCorrectionsHaveBeenApplied)
				{
					((Control)cmbCamera).set_BackColor(Color.DarkGreen);
				}
				else
				{
					((Control)cmbCamera).set_BackColor(Color.Red);
				}
				((Control)cmbCamera).set_ForeColor(Color.Yellow);
				((Control)cmbCamera).Refresh();
				((ListControl)cmbFrames).set_SelectedIndex(0);
				((Control)cmdDisplayReport).Focus();
				((Control)grpCameraCorrections).set_Enabled(true);
			}
			else
			{
				((Control)cmbCamera).set_BackColor(SystemColors.Window);
				((Control)cmbCamera).set_ForeColor(SystemColors.ControlText);
				((Control)cmbCamera).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular));
			}
		}

		private void cmdSetLevelsAutomatic_Click(object sender, EventArgs e)
		{
			SetLevelsAutomatically();
		}

		internal void SetLevelsAutomatically()
		{
			AOTAData.GetMean_SD(0, AOTAData.StarCountToPlot, out var Mean, out var SD, out var Variance);
			AOTAData.GetUpperLowerMeans(Mean - SD, out var UpperMean, out var LowerMean);
			AOTAData.YFull = UpperMean;
			AOTAData.XFull = 25;
			AOTAData.YFullSD = (AOTAData.YOccSD = SD);
			AOTAData.YFullVariance = (AOTAData.YOccVariance = Variance);
			MagDropCalculator(UpdateMagDrop: false);
			((Control)txtFullPosn).set_Text(AOTAData.XFull.ToString());
			AOTAData.YOcc = (int)LowerMean;
			if (AOTAData.YOcc == 0.0)
			{
				AOTAData.YOcc = 1.0;
			}
			AOTAData.XOcc = AOTAData.StarCountToPlot / 2;
			((Control)txtOccLevelPos).set_Text(AOTAData.XOcc.ToString());
			((Control)txtFullLightLevel).set_Text(string.Format("{0,1:f1}", UpperMean));
			((Control)txtOccultedLightLevel).set_Text(string.Format("{0,1:f1}", LowerMean));
			AOTAData.PlotAOTA();
		}

		private void updnMagStar_ValueChanged(object sender, EventArgs e)
		{
			MagDropCalculator(UpdateMagDrop: true);
		}

		private void updnMagObject_ValueChanged(object sender, EventArgs e)
		{
			MagDropCalculator(UpdateMagDrop: true);
		}

		private void updnMagDrop_ValueChanged(object sender, EventArgs e)
		{
			MagDropCalculator(UpdateMagDrop: false);
			if (tabPlot.get_SelectedIndex() == 3)
			{
				updnExpectedMagDrop.set_Value(updnMagDrop.get_Value());
			}
		}

		internal void MagDropCalculator(bool UpdateMagDrop)
		{
			double num = (double)updnMagStar.get_Value();
			double num2 = (double)updnMagObject.get_Value();
			double num3 = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) + Math.Pow(10.0, num2 / -2.5));
			if (UpdateMagDrop)
			{
				updnMagDrop.set_Value((decimal)(num2 - num3));
			}
			double num4 = Math.Pow(10.0, (double)updnMagDrop.get_Value() / 2.5);
			AOTAData.YExpected = (float)(AOTAData.YFull / num4);
			((Control)txtMagDropLightLevel).set_Text(string.Format("{0,1:F1}", AOTAData.YExpected));
			AOTAData.PlotAOTA();
		}

		private void cmdCancelFind_MouseDown(object sender, MouseEventArgs e)
		{
			AOTAData.CancelFind = true;
		}

		private void cmdCancelFind_Click(object sender, EventArgs e)
		{
			AOTAData.CancelFind = true;
		}

		private void cmdCancelFind_KeyDown(object sender, KeyEventArgs e)
		{
			AOTAData.CancelFind = true;
		}

		private void cmdCancelFind_MouseClick(object sender, MouseEventArgs e)
		{
			AOTAData.CancelFind = true;
		}

		internal void ResetLevels()
		{
			optFullLight.set_Checked(true);
			TextBox obj = txtFullLightLevel;
			TextBox obj2 = txtFullPosn;
			TextBox obj3 = txtOccLevelPos;
			TextBox obj4 = txtOccultedLightLevel;
			string text;
			((Control)txtExpectedSN).set_Text(text = "0");
			string text2;
			((Control)obj4).set_Text(text2 = text);
			string text3;
			((Control)obj3).set_Text(text3 = text2);
			string text4;
			((Control)obj2).set_Text(text4 = text3);
			((Control)obj).set_Text(text4);
		}

		private void cmdGetDandR_Click(object sender, EventArgs e)
		{
			AnalyseForDandR(0, 0);
			AOTAData.AutoProcess = false;
		}

		internal void AnalyseForDandR(int Dselected, int Rselected)
		{
			Chi2.InitialiseVariables();
			Chi2.Get_Chi2(Disappear: true);
			Chi2.Get_Chi2(Disappear: false);
			AOTAData.PlotChi2();
			GetDataForDslope(Dselected);
			GetDataForRslope(Rselected);
			if (AOTAData.FirstTimeAnalysed)
			{
				if (AOTAData.MinimumChi2DValue < 0.8 * Chi2.Chi2_D_Minimums[0])
				{
					GetDataForDslope(AOTAData.MinimumChi2DPosition);
				}
				else if (Chi2.Chi2_D_Minimums[1] < 0.95 * Chi2.Chi2_D_Minimums[0])
				{
					GetDataForDslope(1);
				}
				else
				{
					GetDataForDslope(0);
				}
				if (AOTAData.MinimumChi2RValue < 0.8 * Chi2.Chi2_R_Minimums[0])
				{
					GetDataForRslope(AOTAData.MinimumChi2RPosition);
				}
				else if (Chi2.Chi2_R_Minimums[1] < 0.95 * Chi2.Chi2_R_Minimums[0])
				{
					GetDataForRslope(1);
				}
				else
				{
					GetDataForRslope(0);
				}
				AOTAData.PlotForm.tabPlot.set_SelectedIndex(6);
				AOTAData.FirstTimeAnalysed = false;
			}
		}

		private void DisplayEvent_D_Frame(int SelectedSlope)
		{
			AOTAData.GetUncertaintyLimits();
			if (AOTAData.MissEvent[AOTAData.CurrentSolution])
			{
				AOTAData.Results[AOTAData.CurrentSolution].D_Frame = (AOTAData.Results[AOTAData.CurrentSolution].R_Frame = (AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertPlus = (AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertMinus = (AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertPlus = (AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertMinus = (AOTAData.Results[AOTAData.CurrentSolution].D_DurationFrames = (AOTAData.Results[AOTAData.CurrentSolution].R_DurationFrames = -1f)))))));
				AOTAData.Results[AOTAData.CurrentSolution].D_UTC = (AOTAData.Results[AOTAData.CurrentSolution].R_UTC = "-1");
				return;
			}
			float num = 0.5f;
			float num2 = 0f;
			float num3 = 0f;
			float num4 = AOTAData.Chi2D_Selected;
			if (SelectedSlope % 2 == 1)
			{
				num = 0f;
			}
			((Control)lblDframe).set_ForeColor(Color.Black);
			double num9;
			double num10;
			if (AOTAData.PlusLimit[0] != 0f)
			{
				if ((AOTAData.PlusLimit[0] > 5f) | (AOTAData.MinusLimit[0] < -5f))
				{
					((Control)lblDframe).set_ForeColor(Color.Maroon);
				}
				if (((double)AOTAData.PlusLimit[0] > 9.5) | ((double)AOTAData.MinusLimit[0] < -9.5))
				{
					((Control)lblDframe).set_ForeColor(Color.Red);
				}
				num2 = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + AOTAData.MinusLimit[0]);
				num3 = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + AOTAData.PlusLimit[0]);
				AOTAData.Results[AOTAData.CurrentSolution].D_Frame = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (float)AOTAData.NumberOfFramesBinned * (0.5f + num);
				AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertPlus = (float)AOTAData.NumberOfFramesBinned * AOTAData.PlusLimit[0];
				AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertMinus = (float)AOTAData.NumberOfFramesBinned * AOTAData.MinusLimit[0];
				((Control)lblDFrameFinal).set_Text("at frame " + string.Format("{0,1:f1}  +{1,2:f1}/{2,1:f1}", (double)AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (double)AOTAData.NumberOfFramesBinned * (0.5 + (double)num), (float)AOTAData.NumberOfFramesBinned * AOTAData.PlusLimit[0], (float)AOTAData.NumberOfFramesBinned * AOTAData.MinusLimit[0]));
				((Control)lblDframe).set_Text(string.Format("D: {0,1:F1} - {1,1:F1}; Dur. {2,1:f0}", num2, num3, num4));
				float num5 = (float)AOTAData.FirstFrameUsedInAnalysis + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + (float)Chi2.Chi2_D_MinimumPosns[SelectedSlope] + AOTAData.MinusLimit[0]);
				float num6 = (float)AOTAData.FirstFrameUsedInAnalysis + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + (float)Chi2.Chi2_D_MinimumPosns[SelectedSlope] + AOTAData.PlusLimit[0]);
				if (num5 < 0f)
				{
					num5 = 0f;
				}
				if (num6 >= (float)AOTAData.StarCount)
				{
					num6 = AOTAData.StarCount - 1;
				}
				if (num6 < 0f)
				{
					num6 = 0f;
				}
				double num7 = AOTAData.FrameTimeSecs[(int)Math.Floor(num5)] + AOTAData.FrameTimeCorrnsD[AOTAData.CurrentSolution];
				if (num5 % 1f > 0f)
				{
					num7 = (num7 + AOTAData.FrameTimeSecs[(int)Math.Floor(num5) + 1]) / 2.0;
				}
				double num8 = AOTAData.FrameTimeSecs[(int)Math.Floor(num6)] + AOTAData.FrameTimeCorrnsD[AOTAData.CurrentSolution];
				if (num6 % 1f > 0f)
				{
					num8 = (num8 + AOTAData.FrameTimeSecs[(int)Math.Floor(num6) + 1]) / 2.0;
				}
				num9 = (num7 + num8) / 2.0 - CameraDelaySecs;
				num10 = (num8 - num7) / 2.0;
			}
			else
			{
				AOTAData.Results[AOTAData.CurrentSolution].D_Frame = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis] + 0.5f;
				AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertPlus = (AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertMinus = -1f);
				((Control)lblDFrameFinal).set_Text("at frame " + string.Format("{0,1:f1}", AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis] + 0.5f));
				((Control)lblDframe).set_Text("D " + ((Control)lblDFrameFinal).get_Text());
				float num5 = (float)(AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis) + 0.5f;
				num9 = AOTAData.FrameTimeSecs[(int)Math.Floor(num5)] + AOTAData.FrameTimeCorrnsD[AOTAData.CurrentSolution] - CameraDelaySecs;
				if (num5 < 0f)
				{
					num5 = 0f;
				}
				if (num5 % 1f > 0f)
				{
					num9 = (num9 + AOTAData.FrameTimeSecs[(int)Math.Floor(num5) + 1]) / 2.0 - CameraDelaySecs;
				}
				num10 = (AOTAData.FrameTimeSecs[1] - AOTAData.FrameTimeSecs[0]) / 2.0;
			}
			SetTimeSource();
			int decimalLength = Utilities.SignificantDigitLocation_DecimalNumbers(num10);
			((Control)lbl_UT_D).set_Text(Utilities.DEGtoDMS(num9 / 3600.0, 2, decimalLength, MinutesOnly: false));
			((Control)lbl_UTuncert_D).set_Text(string.Format("{0,1:f" + decimalLength + "}", num10));
			((Control)lblDtransition).set_Text(string.Format("{0,1:f0} frames", num4 * (float)AOTAData.NumberOfFramesBinned));
			AOTAData.Results[AOTAData.CurrentSolution].D_UTC = ((Control)lbl_UT_D).get_Text() + " ± " + ((Control)lbl_UTuncert_D).get_Text();
			AOTAData.Results[AOTAData.CurrentSolution].D_DurationFrames = num4 * (float)AOTAData.NumberOfFramesBinned;
			AOTAData.MissEvent[AOTAData.CurrentSolution] = false;
			AOTAData.AppliedConfidenceD[AOTAData.CurrentSolution] = AOTAData.ConfidenceLevelD;
			AOTAData.InvalidNearD[AOTAData.CurrentSolution] = 0;
			int num11 = AOTAData.NumberOfFramesBinned * Chi2.Chi2_D_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis;
			for (int i = num11 - 10; i < num11 + 11; i++)
			{
				if (!((i < 0) | (i >= AOTAData.StarCountToPlot)) && !AOTAData.StarValidToPlot[i])
				{
					AOTAData.InvalidNearD[AOTAData.CurrentSolution]++;
				}
			}
		}

		private void DisplayEvent_R_Frame(int SelectedSlope)
		{
			AOTAData.GetUncertaintyLimits();
			if (AOTAData.MissEvent[AOTAData.CurrentSolution])
			{
				AOTAData.Results[AOTAData.CurrentSolution].D_Frame = (AOTAData.Results[AOTAData.CurrentSolution].R_Frame = (AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertPlus = (AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertMinus = (AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertPlus = (AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertMinus = (AOTAData.Results[AOTAData.CurrentSolution].D_DurationFrames = (AOTAData.Results[AOTAData.CurrentSolution].R_DurationFrames = -1f)))))));
				AOTAData.Results[AOTAData.CurrentSolution].D_UTC = (AOTAData.Results[AOTAData.CurrentSolution].R_UTC = "-1");
				return;
			}
			float num = 0.5f;
			float num2 = 0f;
			float num3 = 0f;
			float num4 = AOTAData.Chi2R_Selected;
			if (SelectedSlope % 2 == 1)
			{
				num = 0f;
			}
			((Control)lblRframe).set_ForeColor(Color.Black);
			double num9;
			double num10;
			if (AOTAData.PlusLimit[1] != 0f)
			{
				if ((AOTAData.PlusLimit[1] > 5f) | (AOTAData.MinusLimit[1] < -5f))
				{
					((Control)lblRframe).set_ForeColor(Color.Maroon);
				}
				if (((double)AOTAData.PlusLimit[1] > 9.5) | ((double)AOTAData.MinusLimit[1] < -9.5))
				{
					((Control)lblRframe).set_ForeColor(Color.Red);
				}
				num2 = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + AOTAData.MinusLimit[1]);
				num3 = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + AOTAData.PlusLimit[1]);
				((Control)lblRframe).set_Text(string.Format("R: {0,1:F1} - {1,1:F1}; Dur. {2,1:f0}", num2, num3, num4));
				AOTAData.Results[AOTAData.CurrentSolution].R_Frame = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (float)AOTAData.NumberOfFramesBinned * (0.5f + num);
				AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertPlus = (float)AOTAData.NumberOfFramesBinned * AOTAData.PlusLimit[1];
				AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertMinus = (float)AOTAData.NumberOfFramesBinned * AOTAData.MinusLimit[1];
				((Control)lblRFrameFinal).set_Text("at frame " + string.Format("{0,1:f1}  +{1,2:f1}/{2,1:f1}", (double)AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[SelectedSlope] + AOTAData.FirstFrameUsedInAnalysis] + (double)AOTAData.NumberOfFramesBinned * (0.5 + (double)num), (float)AOTAData.NumberOfFramesBinned * AOTAData.PlusLimit[1], (float)AOTAData.NumberOfFramesBinned * AOTAData.MinusLimit[1]));
				float num5 = (float)AOTAData.FirstFrameUsedInAnalysis + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + (float)Chi2.Chi2_R_MinimumPosns[SelectedSlope] + AOTAData.MinusLimit[1]);
				float num6 = (float)AOTAData.FirstFrameUsedInAnalysis + (float)AOTAData.NumberOfFramesBinned * (0.5f + num + (float)Chi2.Chi2_R_MinimumPosns[SelectedSlope] + AOTAData.PlusLimit[1]);
				if (num5 < 0f)
				{
					num5 = 0f;
				}
				if (num6 >= (float)AOTAData.StarCount)
				{
					num6 = AOTAData.StarCount - 1;
				}
				if (num6 < 0f)
				{
					num6 = 0f;
				}
				double num7 = AOTAData.FrameTimeSecs[(int)Math.Floor(num5)] + AOTAData.FrameTimeCorrnsR[AOTAData.CurrentSolution];
				if (num5 % 1f > 0f)
				{
					num7 = (num7 + AOTAData.FrameTimeSecs[(int)Math.Floor(num5) + 1]) / 2.0;
				}
				double num8 = AOTAData.FrameTimeSecs[(int)Math.Floor(num6)] + AOTAData.FrameTimeCorrnsR[AOTAData.CurrentSolution];
				if (num6 % 1f > 0f)
				{
					num8 = (num8 + AOTAData.FrameTimeSecs[(int)Math.Floor(num6) + 1]) / 2.0;
				}
				num9 = (num7 + num8) / 2.0 - CameraDelaySecs;
				num10 = (num8 - num7) / 2.0;
			}
			else
			{
				AOTAData.Results[AOTAData.CurrentSolution].R_Frame = AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis] + 0.5f;
				AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertPlus = (AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertMinus = -1f);
				((Control)lblRFrameFinal).set_Text("at frame " + string.Format("{0,1:f1}", AOTAData.FrameID[AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis] + 0.5f));
				((Control)lblRframe).set_Text("R " + ((Control)lblRFrameFinal).get_Text());
				float num5 = (float)(AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis) + 0.5f;
				if (num5 < 0f)
				{
					num5 = 0f;
				}
				num9 = AOTAData.FrameTimeSecs[(int)Math.Floor(num5)] + AOTAData.FrameTimeCorrnsR[AOTAData.CurrentSolution];
				if (num5 % 1f > 0f)
				{
					num9 = (num9 + AOTAData.FrameTimeSecs[(int)Math.Floor(num5) + 1]) / 2.0 - CameraDelaySecs;
				}
				num10 = (AOTAData.FrameTimeSecs[1] - AOTAData.FrameTimeSecs[0]) / 2.0 - CameraDelaySecs;
			}
			SetTimeSource();
			int decimalLength = Utilities.SignificantDigitLocation_DecimalNumbers(num10);
			((Control)lbl_UT_R).set_Text(Utilities.DEGtoDMS(num9 / 3600.0, 2, decimalLength, MinutesOnly: false));
			((Control)lbl_UTuncert_R).set_Text(string.Format("{0,1:f" + decimalLength + "}", num10));
			((Control)lblRtransition).set_Text(string.Format("{0,1:f0} frames", num4 * (float)AOTAData.NumberOfFramesBinned));
			AOTAData.Results[AOTAData.CurrentSolution].R_UTC = ((Control)lbl_UT_R).get_Text() + " ± " + ((Control)lbl_UTuncert_R).get_Text();
			AOTAData.Results[AOTAData.CurrentSolution].R_DurationFrames = num4 * (float)AOTAData.NumberOfFramesBinned;
			AOTAData.AppliedConfidenceR[AOTAData.CurrentSolution] = AOTAData.ConfidenceLevelR;
			AOTAData.InvalidNearR[AOTAData.CurrentSolution] = 0;
			int num11 = AOTAData.NumberOfFramesBinned * Chi2.Chi2_R_MinimumPosns[0] + AOTAData.FirstFrameUsedInAnalysis;
			for (int i = num11 - 10; i < num11 + 11; i++)
			{
				if (!((i < 0) | (i >= AOTAData.StarCountToPlot)) && !AOTAData.StarValidToPlot[i])
				{
					AOTAData.InvalidNearR[AOTAData.CurrentSolution]++;
				}
			}
		}

		private void SetTimeSource()
		{
			if (AOTAData.TimeSource == 0)
			{
				((Control)lblTimeSource).set_Text("Manual");
			}
			else if (AOTAData.TimeSource == 1)
			{
				((Control)lblTimeSource).set_Text("Tangra");
			}
			else if (AOTAData.TimeSource == 2)
			{
				((Control)lblTimeSource).set_Text("LiMovie");
			}
			else if (AOTAData.TimeSource == 3)
			{
				((Control)lblTimeSource).set_Text("PyMovie");
			}
		}

		private void cmdGetFrameOf_D_Click(object sender, EventArgs e)
		{
			Chi2.Get_Chi2(Disappear: true);
		}

		private void cmdGetFrameOf_R_Click(object sender, EventArgs e)
		{
			Chi2.Get_Chi2(Disappear: false);
		}

		private void picChiD_MouseClick(object sender, MouseEventArgs e)
		{
			float num = e.get_X();
			GetDataForDslope((int)(Math.Floor(num / AOTAData.ChiPlot_HorizontalScale) - 1.0));
		}

		internal void GetDataForDslope(int DSlope)
		{
			AOTAData.Chi2D_Selected = DSlope;
			AOTAData.PlotChi2();
			AOTAData.PlotAOTA();
			Chi2.GetUncertainty(DSlope, Disappear: true);
			AOTAData.GetUncertaintyLimits();
			if ((AOTAData.Chi2D_Selected >= 0) & (AOTAData.Chi2D_Selected <= Chi2.Chi2_D_MinimumPosns.GetUpperBound(0)))
			{
				DisplayEvent_D_Frame(AOTAData.Chi2D_Selected);
			}
			else
			{
				((Control)lblDframe).set_Text("D");
			}
			AOTAData.PlotAOTA();
		}

		private void picChiR_MouseClick(object sender, MouseEventArgs e)
		{
			float num = e.get_X();
			GetDataForRslope((int)(Math.Floor(num / AOTAData.ChiPlot_HorizontalScale) - 1.0));
		}

		internal void GetDataForRslope(int RSlope)
		{
			AOTAData.Chi2R_Selected = RSlope;
			AOTAData.PlotChi2();
			AOTAData.PlotAOTA();
			Chi2.GetUncertainty(RSlope, Disappear: false);
			AOTAData.GetUncertaintyLimits();
			if ((AOTAData.Chi2R_Selected >= 0) & (AOTAData.Chi2D_Selected <= Chi2.Chi2_D_MinimumPosns.GetUpperBound(0)))
			{
				DisplayEvent_R_Frame(AOTAData.Chi2R_Selected);
			}
			else
			{
				((Control)lblRframe).set_Text("R");
			}
			AOTAData.PlotAOTA();
		}

		private void updnExtraPoints_ValueChanged(object sender, EventArgs e)
		{
			if (!AOTAData.UpdatingExtraPointsUsedInSolution)
			{
				SetRanges();
			}
		}

		private void SetRanges()
		{
			AOTAData.ExtraPointsUsedInSolution = (int)updnExtraPoints.get_Value();
			AOTAData.X0 = AOTAData.X1 - AOTAData.ExtraPointsUsedInSolution;
			AOTAData.X3 = AOTAData.X2 + AOTAData.ExtraPointsUsedInSolution;
			AOTAData.X4 = AOTAData.X5 - AOTAData.ExtraPointsUsedInSolution;
			AOTAData.X7 = AOTAData.X6 + AOTAData.ExtraPointsUsedInSolution;
			if (AOTAData.X3 > AOTAData.X5)
			{
				AOTAData.X3 = AOTAData.X5;
			}
			if (AOTAData.X4 < AOTAData.X2)
			{
				AOTAData.X4 = AOTAData.X2;
			}
			AnalyseForDandR(AOTAData.Chi2D_Selected, AOTAData.Chi2R_Selected);
		}

		private void cmdTest_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveForm();
			AOTAData.PlotLightCurve();
		}

		private void updnMonteCarlo_ValueChanged(object sender, EventArgs e)
		{
			Chi2.MonteCarloTrials = (int)updnMonteCarlo.get_Value();
			AnalyseForDandR(AOTAData.Chi2D_Selected, AOTAData.Chi2R_Selected);
		}

		private void updnTransitionNumber_ValueChanged(object sender, EventArgs e)
		{
			AOTAData.MaximimumTransitions = (int)updnTransitionNumber.get_Value();
			if (AOTAData.Chi2D_Selected >= AOTAData.MaximimumTransitions)
			{
				AOTAData.Chi2D_Selected = AOTAData.MaximimumTransitions;
			}
			if (AOTAData.Chi2R_Selected >= AOTAData.MaximimumTransitions)
			{
				AOTAData.Chi2R_Selected = AOTAData.MaximimumTransitions;
			}
			Chi2.InitialiseVariables();
			Chi2.CreateTransitionSignals();
			AnalyseForDandR(AOTAData.Chi2D_Selected, AOTAData.Chi2R_Selected);
		}

		private void updnSDLimit_ValueChanged(object sender, EventArgs e)
		{
			Gaussian.MultipleOfSD = (double)updnSDLimit.get_Value();
			AnalyseForDandR(AOTAData.Chi2D_Selected, AOTAData.Chi2R_Selected);
		}

		private void cmd_x1_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(1m);
		}

		private void cmd_x5_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(5m);
		}

		private void cmd_x10_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(10m);
		}

		private void cmd_x15_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(15m);
		}

		private void updnVerticalScaleAdjustment_ValueChanged(object sender, EventArgs e)
		{
			AOTAData.VerticalScaleAdjustment = (float)updnVerticalScaleAdjustment.get_Value();
			AOTAData.PlotAOTA();
		}

		private void savePlotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!Utilities.ScreenSizeTooLargeForImageSaves())
			{
				string fileRootName = "";
				if (AOTA_ExternalAccess.RunFromTangra)
				{
					fileRootName = Path.GetDirectoryName(AOTAData.TangraSourceFile) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.TangraSourceFile) + "_Bin" + AOTAData.NumberOfFramesBinned + "_Plot";
				}
				else if (AOTAData.CurrentFileName.Trim() != "")
				{
					fileRootName = Path.GetDirectoryName(AOTAData.CurrentFileName) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.CurrentFileName) + "_Bin" + AOTAData.NumberOfFramesBinned + "_Plot";
				}
				Settings.Default.Save_Plot = Output.SaveGraphic(picPlot.get_Image(), fileRootName, Settings.Default.Save_Plot);
			}
		}

		private void saveFourierToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string fileRootName = "";
			if (AOTA_ExternalAccess.RunFromTangra)
			{
				fileRootName = Path.GetDirectoryName(AOTAData.TangraSourceFile) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.TangraSourceFile) + "_Bin" + AOTAData.NumberOfFramesBinned + "_Fourier";
			}
			else if (AOTAData.CurrentFileName.Trim() != "")
			{
				fileRootName = Path.GetDirectoryName(AOTAData.CurrentFileName) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.CurrentFileName) + "_Bin" + AOTAData.NumberOfFramesBinned + "_Fourier";
			}
			Settings.Default.Save_Fourier = Output.SaveGraphic(picFourier.get_Image(), fileRootName, Settings.Default.Save_Fourier);
		}

		private void saveImageOfFormToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_01d2: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			double num = (double)Settings.Default.MonitorScale / 100.0;
			int width = (int)(num * (double)((Control)this).get_Bounds().Width);
			int height = (int)(num * (double)((Control)this).get_Bounds().Height);
			Bitmap bitmap = new Bitmap(width, height);
			Graphics.FromImage(bitmap).CopyFromScreen((int)(num * (double)((Form)this).get_Location().X), (int)(num * (double)((Form)this).get_Location().Y), 0, 0, bitmap.Size);
			try
			{
				if (!AOTA_ExternalAccess.RunFromTangra)
				{
					text = ((!(AOTAData.CurrentFileName.Trim() != "")) ? (Utilities.AppPath + "\\Observations\\CSV Light curve " + DateTime.UtcNow.ToShortDateString() + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Event#" + AOTAData.EventID) : (Path.GetDirectoryName(AOTAData.CurrentFileName) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.CurrentFileName) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Event#" + AOTAData.EventID));
				}
				else
				{
					if (AOTAData.TangraSourceFile == "" && !AOTAData.SetTangraSaveRootFileName())
					{
						return;
					}
					text = Path.GetDirectoryName(AOTAData.TangraSourceFile) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.TangraSourceFile) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Event#" + AOTAData.EventID;
				}
				Output.SaveGraphic(bitmap, text, Path.GetDirectoryName(text));
			}
			catch
			{
				MessageBox.Show("Error - file not saved");
			}
		}

		private void setScalecurrently100ToMatchMonitorScaleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new MonitorScale()).ShowDialog();
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Text("Set scale (currently " + Settings.Default.MonitorScale + "%) to match Monitor scale");
			((Control)lblMonitorScale).set_Text("Monitor scale: " + Settings.Default.MonitorScale + "%");
		}

		private void saveFormAsAnImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void copyPlotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picPlot.get_Image());
		}

		private void copyFourierTransformOfLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picFourier.get_Image());
		}

		private void copyFormToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Bitmap bitmap = new Bitmap(((Control)this).get_Bounds().Width, ((Control)this).get_Bounds().Height);
			Graphics.FromImage(bitmap).CopyFromScreen(((Form)this).get_Location().X, ((Form)this).get_Location().Y, 0, 0, bitmap.Size);
			Clipboard.SetImage((Image)bitmap);
		}

		private void chkShowCrossCorr_CheckedChanged(object sender, EventArgs e)
		{
			AOTAData.PlotAOTA();
		}

		private void chkShowMeasurementMeans_CheckedChanged(object sender, EventArgs e)
		{
			AOTAData.PlotAOTA();
		}

		private void chkShowErrorBars_CheckedChanged(object sender, EventArgs e)
		{
			AOTAData.PlotAOTA();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (tabPlot.get_SelectedIndex() == 0)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA read file");
			}
			else if (tabPlot.get_SelectedIndex() == 1)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA Fourier");
			}
			else if (tabPlot.get_SelectedIndex() == 2)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA Short");
			}
			else if (tabPlot.get_SelectedIndex() == 3)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA find events");
			}
			else if (tabPlot.get_SelectedIndex() == 4)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA select events");
			}
			else if (tabPlot.get_SelectedIndex() == 5)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA analyse");
			}
			else if (tabPlot.get_SelectedIndex() == 6)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA final times");
			}
		}

		private void hintsAndTipsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Hints and Tips");
		}

		private void cmbCamera_Click(object sender, EventArgs e)
		{
		}

		private void cmbCamera_Enter(object sender, EventArgs e)
		{
		}

		private void cmbCamera_Leave(object sender, EventArgs e)
		{
			//IL_003f: Unknown result type (might be due to invalid IL or missing references)
			if ((AOTAData.CameraSpecs.CameraType.Contains("AAV") | AOTAData.CameraSpecs.CameraType.Contains("ADVS")) && AOTAData.SetCameraDelays_AAV_Files)
			{
				MessageBox.Show("AAV and ADVS are not types of cameras. To incorporate delays inherent in the camera, please select the relevant camera from the list\r\n\r\nSet AAV or ADVS using the radio button under Video System", "Select camera type", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			SetCameraBackgroundForAAV();
		}

		private void cmbCamera_SelectedIndexChanged(object sender, EventArgs e)
		{
			//IL_00e4: Unknown result type (might be due to invalid IL or missing references)
			if (CameraChanging)
			{
				return;
			}
			CameraChanging = true;
			Settings.Default.CameraIndex = ((ListControl)cmbCamera).get_SelectedIndex();
			AOTAData.CameraSpecs.CameraType = cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString();
			if (AOTAData.CameraSpecs.CameraType.Contains("AAV") | AOTAData.CameraSpecs.CameraType.Contains("ADVS"))
			{
				if (AOTAData.SetCameraDelays_AAV_Files | !AOTAData.CameraCorrectionsHaveBeenApplied)
				{
					if (AOTAData.CameraSpecs.CameraType.Contains("AAV"))
					{
						AOTAData.CameraSpecs.CameraType = "AAV - specify camera";
					}
					if (AOTAData.CameraSpecs.CameraType.Contains("ADVS"))
					{
						AOTAData.CameraSpecs.CameraType = "ADVS - specify camera";
					}
					MessageBox.Show("AAV and ADVS are not types of cameras. To incorporate delays inherent in the camera, please select the relevant camera from the list\r\n\r\nSet AAV or ADVS using the radio button under Video System", "Select camera type", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
				else
				{
					if (AOTAData.CameraSpecs.CameraType.Contains("AAV"))
					{
						AOTAData.CameraSpecs.CameraType = "AAV - corrected";
					}
					if (AOTAData.CameraSpecs.CameraType.Contains("ADVS"))
					{
						AOTAData.CameraSpecs.CameraType = "ADVS - corrected";
					}
				}
				int selectedIndex = ((ListControl)cmbCamera).get_SelectedIndex();
				cmbCamera.get_Items().Insert(selectedIndex, (object)AOTAData.CameraSpecs.CameraType);
				cmbCamera.get_Items().RemoveAt(selectedIndex + 1);
				((ListControl)cmbCamera).set_SelectedIndex(selectedIndex);
			}
			SetCameraBackgroundForAAV();
			if (((Control)grpCameraCorrections).get_Enabled())
			{
				AOTAData.CameraSpecs.CameraDelaysKnownToAOTA = !AOTAData.CameraSpecs.CameraType.ToLower().Contains("unknown");
			}
			CameraChanging = false;
			SetDelayTimes();
			((Control)cmbFrames).Focus();
		}

		private void cmbFrames_SelectedIndexChanged(object sender, EventArgs e)
		{
			//IL_005a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0081: Unknown result type (might be due to invalid IL or missing references)
			if (!IsLoading)
			{
				int num = int.Parse(cmbFrames.get_Items().get_Item(((ListControl)cmbFrames).get_SelectedIndex()).ToString());
				if (num == 6 || num == 12 || num == 24 || num == 48 || num == 96)
				{
					MessageBox.Show("The selected # frames integrated is only valid\r\nfor certain cameras such as the Mintron12VIC-EX\r\nand the Mallincam MCH.\r\n\r\nAdditionally further corrections are required, as these\r\ncameras perform 'differently' at these settings.\r\n\r\nSee the timing diagrams at\r\n http://www.dangl.at/ausruest/vid_tim/vid_tim1.htm \r\nfor further details.", "Unusual integration setting", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
				if ((AOTAData.IsADVSFile | AOTAData.IsAAVFile) && AOTAData.SetCameraDelays_AAV_Files)
				{
					MessageBox.Show("You are measuring a AAV or ADVS recording. Make sure you:\r\n\r\n1. select the camera used to make the recording, and\r\n\r\n2. set the number of frames integrated BY THE CAMERA", "", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
				SetDelayTimes();
				CheckIntegrationMatchesBinning();
			}
		}

		private void CheckIntegrationMatchesBinning()
		{
			//IL_00d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			int num = int.Parse(cmbFrames.get_Items().get_Item(((ListControl)cmbFrames).get_SelectedIndex()).ToString());
			int num2 = AOTAData.NumberOfFramesBinned;
			if (AOTAData.CameraSpecs.MeasuredAtFieldLevel)
			{
				num2 /= 2;
			}
			if (!(num < 2 && num2 == 1))
			{
				if (num > num2)
				{
					text = ((num2 % num != 0) ? ("The measurements have been binned at " + num2 + " frames (see Tab 1), which is inconsistent with the selected number of frames integrated by the camera (" + num + " frames).\r\n\r\nCheck that you have correctly set both the binning, and the camera integration.") : ("The measurements have been binned at " + num2 + " frames (see Tab 1), which is less than the number of frames integrated by the camera (" + num + " frames).\r\n\r\nCheck that you have correctly set the binning."));
					MessageBox.Show(text, "Mismatch between Binning and Integration", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
				else if (num < num2)
				{
					text = ((num == 0 && num2 > 1) ? ("The measurements have been binned at " + num2 + " frames (see Tab 1) suggesting integration. However the camera setting is for no integration.\r\n\r\nCheck that you have correctly set the camera integration.") : ((num2 % num != 0) ? ("The measurements have been binned at " + num2 + " frames (see Tab 1), which is inconsistent with the selected number of frames integrated by the camera  (" + num + " frames).\r\n\r\nCheck that you have correctly set both the binning, and the camera integration.") : ("The measurements have been binned at " + num2 + " frames (see Tab 1), which is a multiple of the selected number of frames integrated by the camera  (" + num + " frames).\r\n\r\nThis is OK if you are deliberately binning at multiples of the integration period.\r\n\r\nOtherwise, check that you have correctly set the camera integration.")));
					MessageBox.Show(text, "Mismatch between Binning and Integration", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
			}
		}

		private void optSystemNTSC_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading && optSystemNTSC.get_Checked())
			{
				((Control)lblSetNTSCorPAL).set_Visible(false);
				AOTAData.CameraSpecs.VideoSystem = "NTSC";
				SetDelayTimes();
			}
		}

		private void optSystemPAL_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading && optSystemPAL.get_Checked())
			{
				((Control)lblSetNTSCorPAL).set_Visible(false);
				AOTAData.CameraSpecs.VideoSystem = "PAL";
				SetDelayTimes();
			}
		}

		private void optSystemNotKnown_CheckedChanged(object sender, EventArgs e)
		{
			if (optSystemNotKnown.get_Checked())
			{
				((Control)lblTangraCameraCorrns).set_Visible(AOTAData.CameraCorrectionsHaveBeenApplied);
				((Control)lblSetNTSCorPAL).set_Visible(true);
				AOTAData.CameraSpecs.VideoSystem = "Not known";
			}
		}

		private void optSystemOther_CheckedChanged(object sender, EventArgs e)
		{
			if (optSystemOther.get_Checked())
			{
				((Control)lblTangraCameraCorrns).set_Visible(AOTAData.CameraCorrectionsHaveBeenApplied);
				((Control)lblSetNTSCorPAL).set_Visible(!AOTAData.CameraCorrectionsHaveBeenApplied);
				AOTAData.CameraSpecs.VideoSystem = "ADVS or AAV";
			}
		}

		internal void SetDelayTimes()
		{
			((Control)grpCameraCorrections).set_Enabled(!AOTAData.CameraCorrectionsHaveBeenApplied);
			int num = (AOTAData.CameraSpecs.FramesIntegrated = int.Parse(cmbFrames.get_Items().get_Item(((ListControl)cmbFrames).get_SelectedIndex()).ToString()));
			if (!(AOTAData.CameraSpecs.VideoSystem == "ADVS or AAV") && Delays[((ListControl)cmbCamera).get_SelectedIndex()].IntegrationDelay == -1.0)
			{
				((ListControl)cmbFrames).set_SelectedIndex(0);
				num = 0;
			}
			double num2 = 0.0;
			double num3 = (double)num * (1.0 + Delays[((ListControl)cmbCamera).get_SelectedIndex()].MultipleIntegrationDelay);
			((Control)lblIntegrationFrameDelay).set_Text(string.Format("{0,1:f0} frames", num3));
			num2 = ((num <= 0) ? Delays[((ListControl)cmbCamera).get_SelectedIndex()].NonIntegrationDelay : Delays[((ListControl)cmbCamera).get_SelectedIndex()].IntegrationDelay);
			((Control)lblCameraDelayFrames).set_Text(string.Format("{0,1:f1} frames", num2));
			if (AOTAData.CameraCorrectionsHaveBeenApplied)
			{
				CameraDelaySecs = 0.0;
			}
			else if (optSystemNTSC.get_Checked())
			{
				CameraDelaySecs = (num3 + num2) / 29.97;
			}
			else
			{
				CameraDelaySecs = (num3 + num2) / 25.0;
			}
			((Control)lblTimeDelaySecs).set_Text(string.Format("{0,1:f3} secs", 0.0 - CameraDelaySecs));
			DisplayEvent_D_Frame(AOTAData.Chi2D_Selected);
			DisplayEvent_R_Frame(AOTAData.Chi2R_Selected);
		}

		private void PlotForm_Shown(object sender, EventArgs e)
		{
		}

		private void cmdSaveResults_Click(object sender, EventArgs e)
		{
			//IL_0220: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			tabPlot.set_SelectedIndex(5);
			double num = (double)Settings.Default.MonitorScale / 100.0;
			int width = (int)(num * (double)((Control)this).get_Bounds().Width);
			int height = (int)(num * (double)((Control)this).get_Bounds().Height);
			Bitmap bitmap = new Bitmap(width, height);
			Graphics.FromImage(bitmap).CopyFromScreen((int)(num * (double)((Form)this).get_Location().X), (int)(num * (double)((Form)this).get_Location().Y), 0, 0, bitmap.Size);
			try
			{
				if (!AOTA_ExternalAccess.RunFromTangra)
				{
					text = ((!(AOTAData.CurrentFileName.Trim() != "")) ? (Utilities.AppPath + "\\Observations\\CSV Light curve " + DateTime.UtcNow.ToShortDateString() + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Event#" + AOTAData.EventID) : (Path.GetDirectoryName(AOTAData.CurrentFileName) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.CurrentFileName) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Event#" + AOTAData.EventID));
				}
				else
				{
					if (AOTAData.TangraSourceFile == "" && !AOTAData.SetTangraSaveRootFileName())
					{
						return;
					}
					text = Path.GetDirectoryName(AOTAData.TangraSourceFile) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.TangraSourceFile) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Event#" + AOTAData.EventID;
				}
				if (chkAddNameToSaves.get_Checked() & (((Control)txtNameToAdd).get_Text().Trim().Length > 0))
				{
					text = text + "_" + ((Control)txtNameToAdd).get_Text().Trim();
				}
				Output.SaveGraphic(bitmap, text, Path.GetDirectoryName(text));
			}
			catch
			{
				MessageBox.Show("Error - file not saved");
			}
			tabPlot.set_SelectedIndex(6);
		}

		private void cmdSaveMissAnalysis_Click(object sender, EventArgs e)
		{
			//IL_0229: Unknown result type (might be due to invalid IL or missing references)
			decimal value = updnScale.get_Value();
			int value2 = ((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).get_Value();
			decimal num = (decimal)((double)((Control)this).get_Width() / (double)AOTAData.StarCountToPlot);
			if (num < updnScale.get_Minimum())
			{
				num = updnScale.get_Minimum();
			}
			if (num > updnScale.get_Maximum())
			{
				num = updnScale.get_Maximum();
			}
			updnScale.set_Value(num);
			SetSizes(ResizeEvent: false);
			((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(0);
			AOTAData.PlotAOTA();
			string text = "";
			if (!AOTA_ExternalAccess.RunFromTangra)
			{
				text = ((!(AOTAData.CurrentFileName.Trim() != "")) ? (Utilities.AppPath + "\\Observations\\CSV Light curve " + DateTime.UtcNow.ToShortDateString() + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Xcorr") : (Path.GetDirectoryName(AOTAData.CurrentFileName) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.CurrentFileName) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Xcorr"));
			}
			else
			{
				if (AOTAData.TangraSourceFile == "" && !AOTAData.SetTangraSaveRootFileName())
				{
					return;
				}
				text = Path.GetDirectoryName(AOTAData.TangraSourceFile) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.TangraSourceFile) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Xcorr";
			}
			if (chkAddNameToSaves.get_Checked() & (((Control)txtNameToAdd).get_Text().Trim().Length > 0))
			{
				text = text + "_" + ((Control)txtNameToAdd).get_Text().Trim();
			}
			tabPlot.set_SelectedIndex(5);
			try
			{
				Output.SaveGraphic(picPlot.get_Image(), text, Path.GetDirectoryName(text));
			}
			catch
			{
				MessageBox.Show("Error - file not saved");
			}
			updnScale.set_Value(value);
			SetSizes(ResizeEvent: false);
			((ScrollProperties)((ScrollableControl)panelPlot).get_HorizontalScroll()).set_Value(value2);
			tabPlot.set_SelectedIndex(6);
			AOTAData.PlotAOTA();
		}

		private void cmdSetMiss_Click(object sender, EventArgs e)
		{
			//IL_0077: Unknown result type (might be due to invalid IL or missing references)
			bool[] missEvent = AOTAData.MissEvent;
			int eventIDnumber = AOTAData.EventIDnumber;
			bool flag;
			chkSetAsMiss[AOTAData.EventIDnumber].set_Checked(flag = true);
			missEvent[eventIDnumber] = flag;
			((Control)lblEventIsMiss).set_Text("Event #" + AOTAData.EventID + " is set\r\nas a Non-Event\r\n(see tab 4)");
			((Control)lblEventIsMiss).set_Visible(AOTAData.MissEvent[AOTAData.EventIDnumber]);
			AOTAData.ResetOutputs(JustOne: true, AOTAData.EventIDnumber);
			AOTAData.MissEvent[AOTAData.EventIDnumber] = true;
			MessageBox.Show("Check that the expected magnitude drop has \r\nbeen correctly set on Tab 4. \r\nThis value is used to estimate the expected SN value.", "Confirm Mag drop", (MessageBoxButtons)0);
			tabPlot.set_SelectedIndex(4);
		}

		private void cmdSaveReport_Click(object sender, EventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0007: Invalid comparison between Unknown and I4
			if ((int)AOTAData.VerifyBeforeExit(Exit: false) == 7)
			{
				return;
			}
			string text = "";
			if (AOTA_ExternalAccess.RunFromTangra)
			{
				if (AOTAData.TangraSourceFile == "" && !AOTAData.SetTangraSaveRootFileName())
				{
					return;
				}
				text = Path.GetDirectoryName(AOTAData.TangraSourceFile) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.TangraSourceFile) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Report";
			}
			else
			{
				text = Path.GetDirectoryName(AOTAData.CurrentFileName) + "\\" + Path.GetFileNameWithoutExtension(AOTAData.CurrentFileName) + "_Bin" + AOTAData.NumberOfFramesBinned + "_AOTA_Report";
			}
			if (chkAddNameToSaves.get_Checked() & (((Control)txtNameToAdd).get_Text().Trim().Length > 0))
			{
				text = text + "_" + ((Control)txtNameToAdd).get_Text().Trim();
			}
			text += ".txt";
			using StreamWriter streamWriter = new StreamWriter(text);
			streamWriter.Write(AOTAData.CreateTextReport());
		}

		private void cmdDisplayReport_Click(object sender, EventArgs e)
		{
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			string text = AOTAData.CreateTextReport();
			DisplayData displayData = new DisplayData();
			((Control)displayData).set_Width(520);
			((ToolStripItem)displayData.cmdCancel).set_Visible(false);
			((ToolStripItem)displayData.cmdOK).set_Visible(false);
			((Control)displayData.txtBox).set_Text(text);
			((Control)displayData).set_Text("AOTA report");
			((TextBoxBase)displayData.txtBox).Select(0, 0);
			((Form)displayData).ShowDialog();
			((Component)(object)displayData).Dispose();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0007: Invalid comparison between Unknown and I4
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_0046: Invalid comparison between Unknown and I4
			if ((int)AOTAData.VerifyBeforeExit(Exit: true) != 7)
			{
				AOTA_ExternalAccess.ResultsAvailable = false;
				((SettingsBase)Settings.Default).Save();
				if ((!AOTAData.All_Events_Are_Non_Events() & !AOTAData.LightCurveReportHasBeenSaved) && (int)MessageBox.Show("Do you want to report the light curve?", "Report light curve", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					ReportLightCurve();
				}
				((Form)this).Close();
			}
		}

		private void withResultsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0007: Invalid comparison between Unknown and I4
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_0046: Invalid comparison between Unknown and I4
			if ((int)AOTAData.VerifyBeforeExit(Exit: true) != 7)
			{
				AOTA_ExternalAccess.ResultsAvailable = true;
				((SettingsBase)Settings.Default).Save();
				if ((!AOTAData.All_Events_Are_Non_Events() & !AOTAData.LightCurveReportHasBeenSaved) && (int)MessageBox.Show("Do you want to report the light curve?", "Report light curve", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					ReportLightCurve();
				}
				((Form)this).Close();
			}
		}

		private void cancelNoResultsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to Cancel the\r\nAOTA analysis and return with no results?", "Confirm no results", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				AOTA_ExternalAccess.ResultsAvailable = false;
				((SettingsBase)Settings.Default).Save();
				((Form)this).Close();
			}
		}

		private void cmdSetAllToMiss_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < 5; i++)
			{
				chkSetAsMiss[i].set_Checked(true);
			}
		}

		private void optChi2Variance_CheckedChanged(object sender, EventArgs e)
		{
			Chi2.InitialiseVariables();
			Chi2.CreateTransitionSignals();
			Chi2.Get_Chi2(Disappear: true);
			Chi2.Get_Chi2(Disappear: false);
			Chi2.GetUncertainty(0, Disappear: true);
			Chi2.GetUncertainty(0, Disappear: false);
		}

		private void optMonteCarloTestSignal_CheckedChanged(object sender, EventArgs e)
		{
			AnalyseForDandR(AOTAData.Chi2D_Selected, AOTAData.Chi2R_Selected);
		}

		private void cmbPlotAverage_SelectedIndexChanged(object sender, EventArgs e)
		{
			AOTAData.PlotRunningAverage = int.Parse(cmbPlotAverage.get_Items().get_Item(((ListControl)cmbPlotAverage).get_SelectedIndex()).ToString());
			if (!IsLoading)
			{
				AOTAData.PlotAOTA();
			}
		}

		private void PlotForm_FormClosing(object sender, FormClosingEventArgs e)
		{
			if (AOTA_ExternalAccess.AOTA_Client != null)
			{
				TellTangraImClosing();
			}
		}

		private void TellTangraImClosing()
		{
			try
			{
				AOTA_ExternalAccess.AOTA_Client.OnAOTAFormClosing();
			}
			catch (Exception value)
			{
				Trace.WriteLine(value);
			}
		}

		private void cmdVerifyTimes_Click(object sender, EventArgs e)
		{
			AOTAData.ShowVerifyTimes();
			AOTAData.VerifyTimes.FrameNoD = (int)((double)AOTAData.Results[AOTAData.CurrentSolution].D_Frame + (double)(AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertPlus + AOTAData.Results[AOTAData.CurrentSolution].D_FrameUncertMinus) / 2.0);
			((Control)AOTAData.VerifyTimes.lblFrameD).set_Text(string.Format("{0,1:f1}", AOTAData.VerifyTimes.FrameNoD));
			AOTAData.VerifyTimes.updnCorrnD.set_Value((decimal)AOTAData.FrameTimeCorrnsD[AOTAData.CurrentSolution]);
			AOTAData.VerifyTimes.updnCorrnR.set_Value((decimal)AOTAData.FrameTimeCorrnsR[AOTAData.CurrentSolution]);
			for (int i = 0; i < AOTAData.StarCountToPlot; i++)
			{
				if ((double)Math.Abs(AOTAData.FrameID[i] - AOTAData.VerifyTimes.FrameNoD) < 0.1)
				{
					AOTAData.VerifyTimes.ArrayIndexD = i;
					((Control)AOTAData.VerifyTimes.lblInternalTimeD).set_Text(Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[i] / 3600.0, 2, 3, MinutesOnly: false));
					break;
				}
			}
			AOTAData.VerifyTimes.FrameNoR = (int)((double)AOTAData.Results[AOTAData.CurrentSolution].R_Frame + (double)(AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertPlus + AOTAData.Results[AOTAData.CurrentSolution].R_FrameUncertMinus) / 2.0);
			((Control)AOTAData.VerifyTimes.lblFrameR).set_Text(string.Format("{0,1:f1}", AOTAData.VerifyTimes.FrameNoR));
			for (int j = 0; j < AOTAData.StarCountToPlot; j++)
			{
				if ((double)Math.Abs(AOTAData.FrameID[j] - AOTAData.VerifyTimes.FrameNoR) < 0.1)
				{
					AOTAData.VerifyTimes.ArrayIndexR = j;
					((Control)AOTAData.VerifyTimes.lblInternalTimeR).set_Text(Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[j] / 3600.0, 2, 3, MinutesOnly: false));
					break;
				}
			}
			AOTAData.VerifyTimes.updnCorrnD.set_Value((decimal)AOTAData.FrameTimeCorrnsD[AOTAData.CurrentSolution]);
			AOTAData.VerifyTimes.SetAdjustedDtime();
			AOTAData.VerifyTimes.updnCorrnR.set_Value((decimal)AOTAData.FrameTimeCorrnsR[AOTAData.CurrentSolution]);
			AOTAData.VerifyTimes.SetAdjustedRtime();
			((Control)AOTAData.VerifyTimes).set_Text("AOTA: Verify Times, Event #" + AOTAData.EventID.ToString());
		}

		private void cmdIntegrityCheck_Click(object sender, EventArgs e)
		{
			AOTAData.ShowIntegrityCheck(FromLightCurve: false);
		}

		private void updnConfidence_ValueChanged(object sender, EventArgs e)
		{
			AOTAData.ConfidenceLevelD = (double)updnConfidenceD.get_Value() / 100.0;
			((Control)updnConfidenceD).set_BackColor(Color.White);
			if (updnConfidenceD.get_Value() < 90m)
			{
				((Control)updnConfidenceD).set_BackColor(Color.LightGreen);
			}
			if (updnConfidenceD.get_Value() < 80m)
			{
				((Control)updnConfidenceD).set_BackColor(Color.Yellow);
			}
			if (updnConfidenceD.get_Value() < 70m)
			{
				((Control)updnConfidenceD).set_BackColor(Color.Gold);
			}
			if (updnConfidenceD.get_Value() < 60m)
			{
				((Control)updnConfidenceD).set_BackColor(Color.HotPink);
			}
			AnalyseForDandR(AOTAData.Chi2D_Selected, AOTAData.Chi2R_Selected);
			AOTAData.PlotAOTA();
		}

		private void updnConfidenceR_ValueChanged(object sender, EventArgs e)
		{
			AOTAData.ConfidenceLevelR = (double)updnConfidenceR.get_Value() / 100.0;
			((Control)updnConfidenceR).set_BackColor(Color.White);
			if (updnConfidenceR.get_Value() < 90m)
			{
				((Control)updnConfidenceR).set_BackColor(Color.LightGreen);
			}
			if (updnConfidenceR.get_Value() < 80m)
			{
				((Control)updnConfidenceR).set_BackColor(Color.Yellow);
			}
			if (updnConfidenceR.get_Value() < 70m)
			{
				((Control)updnConfidenceR).set_BackColor(Color.Gold);
			}
			if (updnConfidenceR.get_Value() < 60m)
			{
				((Control)updnConfidenceR).set_BackColor(Color.HotPink);
			}
			AnalyseForDandR(AOTAData.Chi2D_Selected, AOTAData.Chi2R_Selected);
			AOTAData.PlotAOTA();
		}

		private void openStarDiameterAnalyserToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StarDiameterAnalysis.ShowStarDiameterAnalyser();
			((ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem).set_Enabled(openStarDiameterAnalyserToolStripMenuItem.get_Checked());
		}

		private void copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			if ((AOTAData.X3 - AOTAData.X0 < 20) | (AOTAData.X7 - AOTAData.X4 < 20))
			{
				MessageBox.Show("There must be at least 20 data points at both the D and R regions.\r\nExpand the number of points outside the transition region, and try again");
				return;
			}
			int num = 0;
			StarDiameterAnalyser.Dcount = AOTAData.X3 - AOTAData.X0 + 1;
			StarDiameterAnalyser.Rcount = AOTAData.X7 - AOTAData.X4 + 1;
			num = 0;
			for (int i = AOTAData.X0; i <= AOTAData.X3; i++)
			{
				StarDiameterAnalyser.ObservedD[num] = AOTAData.StarToPlot[i];
				StarDiameterAnalyser.ObservedDTime[num] = AOTAData.FrameIDofPlot[i].ToString();
				num++;
			}
			StarDiameterAnalyser.Dcount = num;
			num = 0;
			for (int j = AOTAData.X4; j <= AOTAData.X7; j++)
			{
				StarDiameterAnalyser.ObservedR[num] = AOTAData.StarToPlot[j];
				StarDiameterAnalyser.ObservedRTime[num] = AOTAData.FrameIDofPlot[j].ToString();
				num++;
			}
			StarDiameterAnalyser.Rcount = num;
			StarDiameterAnalysis.StarDiameter_Analyser.chkPartial.set_Checked(false);
			StarDiameterAnalysis.StarDiameter_Analyser.fromAOTA();
		}

		private void toolTipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowToolTip = !ShowToolTip;
			toolTipToolStripMenuItem.set_Checked(ShowToolTip);
			toolTip.set_Active(ShowToolTip);
		}

		private void cmdCheckIntegration_Click(object sender, EventArgs e)
		{
			AOTAData.CheckIntegrationDuration();
		}

		private void updnExpectedMagDrop_ValueChanged(object sender, EventArgs e)
		{
			if (tabPlot.get_SelectedIndex() == 4)
			{
				updnMagDrop.set_Value(updnExpectedMagDrop.get_Value());
			}
		}

		private void lightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show(LightCurveMessage, "Confirm Light curve reporting", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 7)
			{
				ReportLightCurve();
			}
		}

		private static void ReportLightCurve()
		{
			LightData.ShowLightCurveForm();
			LightData.LightCurveForm.optAsteroid.set_Checked(true);
			AOTAData.SetLightCurvePlotParameters();
			AOTAData.PlotLightCurve();
		}

		private void cmdReportLightCurve_Click(object sender, EventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show(LightCurveMessage, "Confirm Light curve reporting", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 7)
			{
				ReportLightCurve();
			}
		}

		private void chkScaleComparisons_CheckedChanged(object sender, EventArgs e)
		{
			SetChkScaleColors();
			AOTAData.PlotAOTA();
		}

		private void SetChkScaleColors()
		{
			if (chkScaleComparisons.get_Checked())
			{
				((Control)chkScaleComparisons).set_ForeColor(Color.Yellow);
				((Control)chkScaleComparisons).set_BackColor(Color.RoyalBlue);
			}
			else
			{
				((Control)chkScaleComparisons).set_ForeColor(Color.FromArgb(0, 90, 40));
				((Control)chkScaleComparisons).set_BackColor(Color.FloralWhite);
			}
		}

		private void opt9_Click(object sender, EventArgs e)
		{
			opt9.set_Checked(true);
			RadioButton obj = opt17;
			RadioButton obj2 = opt25;
			RadioButton obj3 = opt33;
			RadioButton obj4 = opt41;
			RadioButton obj5 = opt49;
			bool flag;
			opt57.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool @checked;
			obj2.set_Checked(@checked = flag4);
			obj.set_Checked(@checked);
			RunningAverageCount = 9;
			AOTAData.ShortEventsCheck(9, ForLightCurve: false);
		}

		private void opt17_Click(object sender, EventArgs e)
		{
			opt17.set_Checked(true);
			RadioButton obj = opt9;
			RadioButton obj2 = opt25;
			RadioButton obj3 = opt33;
			RadioButton obj4 = opt41;
			RadioButton obj5 = opt49;
			bool flag;
			opt57.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool @checked;
			obj2.set_Checked(@checked = flag4);
			obj.set_Checked(@checked);
			RunningAverageCount = 17;
			AOTAData.ShortEventsCheck(17, ForLightCurve: false);
		}

		private void opt25_Click(object sender, EventArgs e)
		{
			opt25.set_Checked(true);
			RadioButton obj = opt9;
			RadioButton obj2 = opt17;
			RadioButton obj3 = opt33;
			RadioButton obj4 = opt41;
			RadioButton obj5 = opt49;
			bool flag;
			opt57.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool @checked;
			obj2.set_Checked(@checked = flag4);
			obj.set_Checked(@checked);
			RunningAverageCount = 25;
			AOTAData.ShortEventsCheck(25, ForLightCurve: false);
		}

		private void opt33_Click(object sender, EventArgs e)
		{
			opt33.set_Checked(true);
			RadioButton obj = opt9;
			RadioButton obj2 = opt17;
			RadioButton obj3 = opt25;
			RadioButton obj4 = opt41;
			RadioButton obj5 = opt49;
			bool flag;
			opt57.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool @checked;
			obj2.set_Checked(@checked = flag4);
			obj.set_Checked(@checked);
			RunningAverageCount = 33;
			AOTAData.ShortEventsCheck(33, ForLightCurve: false);
		}

		private void opt41_Click(object sender, EventArgs e)
		{
			opt41.set_Checked(true);
			RadioButton obj = opt9;
			RadioButton obj2 = opt17;
			RadioButton obj3 = opt25;
			RadioButton obj4 = opt33;
			RadioButton obj5 = opt49;
			bool flag;
			opt57.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool @checked;
			obj2.set_Checked(@checked = flag4);
			obj.set_Checked(@checked);
			RunningAverageCount = 41;
			AOTAData.ShortEventsCheck(41, ForLightCurve: false);
		}

		private void opt49_Click(object sender, EventArgs e)
		{
			opt49.set_Checked(true);
			RadioButton obj = opt9;
			RadioButton obj2 = opt17;
			RadioButton obj3 = opt25;
			RadioButton obj4 = opt33;
			RadioButton obj5 = opt41;
			bool flag;
			opt57.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool @checked;
			obj2.set_Checked(@checked = flag4);
			obj.set_Checked(@checked);
			RunningAverageCount = 49;
			AOTAData.ShortEventsCheck(49, ForLightCurve: false);
		}

		private void opt57_Click(object sender, EventArgs e)
		{
			opt57.set_Checked(true);
			RadioButton obj = opt9;
			RadioButton obj2 = opt17;
			RadioButton obj3 = opt25;
			RadioButton obj4 = opt33;
			RadioButton obj5 = opt41;
			bool flag;
			opt49.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool @checked;
			obj2.set_Checked(@checked = flag4);
			obj.set_Checked(@checked);
			AOTAData.ShortEventsCheck(57, ForLightCurve: false);
		}

		private void plotUsingLargePointsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			plotUsingLargePointsToolStripMenuItem.set_Checked(!plotUsingLargePointsToolStripMenuItem.get_Checked());
			AOTAData.PlotAOTA();
		}

		private void updnBottom_ValueChanged(object sender, EventArgs e)
		{
			AOTAData.PlotAOTA();
		}

		private void lstTarget_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				string text = "";
				for (int i = 0; i < lstTarget.get_Items().get_Count(); i++)
				{
					text = text + lstTarget.get_Items().get_Item(i).ToString() + "\r\n";
				}
				Clipboard.SetText(text);
				((Control)lblCopied).set_Location(((Control)lstTarget).get_Location());
				((Control)lblCopied).Show();
				Tcopy.Start();
			}
		}

		private void lstComp1_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				string text = "";
				for (int i = 0; i < lstComp1.get_Items().get_Count(); i++)
				{
					text = text + lstComp1.get_Items().get_Item(i).ToString() + "\r\n";
				}
				Clipboard.SetText(text);
				((Control)lblCopied).set_Location(((Control)lstComp1).get_Location());
				((Control)lblCopied).Show();
				Tcopy.Start();
			}
		}

		private void lstComp2_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				string text = "";
				for (int i = 0; i < lstComp2.get_Items().get_Count(); i++)
				{
					text = text + lstComp2.get_Items().get_Item(i).ToString() + "\r\n";
				}
				Clipboard.SetText(text);
				((Control)lblCopied).set_Location(((Control)lstComp2).get_Location());
				((Control)lblCopied).Show();
				Tcopy.Start();
			}
		}

		private void lstComp3_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				string text = "";
				for (int i = 0; i < lstComp3.get_Items().get_Count(); i++)
				{
					text = text + lstComp3.get_Items().get_Item(i).ToString() + "\r\n";
				}
				Clipboard.SetText(text);
				((Control)lblCopied).set_Location(((Control)lstComp3).get_Location());
				((Control)lblCopied).Show();
				Tcopy.Start();
			}
		}

		private void optComp1_Click(object sender, EventArgs e)
		{
			optComp1.set_Checked(!optComp1.get_Checked());
			AOTAData.ShortEventsCheck(RunningAverageCount, ForLightCurve: false);
		}

		private void optComp2_Click(object sender, EventArgs e)
		{
			optComp2.set_Checked(!optComp2.get_Checked());
			AOTAData.ShortEventsCheck(RunningAverageCount, ForLightCurve: false);
		}

		private void optComp3_Click(object sender, EventArgs e)
		{
			optComp3.set_Checked(!optComp3.get_Checked());
			AOTAData.ShortEventsCheck(RunningAverageCount, ForLightCurve: false);
		}

		private void alewaysShowComparisonsAtFileOpenToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			//IL_002a: Invalid comparison between Unknown and I4
			if (!alwaysShowComparisonsAtFileOpenToolStripMenuItem.get_Checked() || (int)MessageBox.Show("By clearing this setting, Comparison star light curves will\r\nnot be displayed by default. This will increase the risk\r\nof failing to recognise that an 'event' might only be noise.\r\r\n\r\nDo you want to clear this check", "Confirm clearoing comparison check", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				alwaysShowComparisonsAtFileOpenToolStripMenuItem.set_Checked(!alwaysShowComparisonsAtFileOpenToolStripMenuItem.get_Checked());
				Settings.Default.AOTAshowComparisons = alwaysShowComparisonsAtFileOpenToolStripMenuItem.get_Checked();
			}
		}

		private void correctForMissingExposuresToolStripMenuItem_Click(object sender, EventArgs e)
		{
			correctForMissingExposuresToolStripMenuItem.set_Checked(!correctForMissingExposuresToolStripMenuItem.get_Checked());
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0570: Unknown result type (might be due to invalid IL or missing references)
			//IL_057a: Expected O, but got Unknown
			//IL_057b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0585: Expected O, but got Unknown
			//IL_0586: Unknown result type (might be due to invalid IL or missing references)
			//IL_0590: Expected O, but got Unknown
			//IL_0591: Unknown result type (might be due to invalid IL or missing references)
			//IL_059b: Expected O, but got Unknown
			//IL_059c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a6: Expected O, but got Unknown
			//IL_05a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Expected O, but got Unknown
			//IL_05b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bc: Expected O, but got Unknown
			//IL_05bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c7: Expected O, but got Unknown
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d2: Expected O, but got Unknown
			//IL_05d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05dd: Expected O, but got Unknown
			//IL_05de: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e8: Expected O, but got Unknown
			//IL_05e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f3: Expected O, but got Unknown
			//IL_05f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fe: Expected O, but got Unknown
			//IL_05ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0609: Expected O, but got Unknown
			//IL_060a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0614: Expected O, but got Unknown
			//IL_0615: Unknown result type (might be due to invalid IL or missing references)
			//IL_061f: Expected O, but got Unknown
			//IL_0620: Unknown result type (might be due to invalid IL or missing references)
			//IL_062a: Expected O, but got Unknown
			//IL_062b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0635: Expected O, but got Unknown
			//IL_0636: Unknown result type (might be due to invalid IL or missing references)
			//IL_0640: Expected O, but got Unknown
			//IL_0641: Unknown result type (might be due to invalid IL or missing references)
			//IL_064b: Expected O, but got Unknown
			//IL_064c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0656: Expected O, but got Unknown
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			//IL_0662: Unknown result type (might be due to invalid IL or missing references)
			//IL_066c: Expected O, but got Unknown
			//IL_066d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0677: Expected O, but got Unknown
			//IL_0678: Unknown result type (might be due to invalid IL or missing references)
			//IL_0682: Expected O, but got Unknown
			//IL_0683: Unknown result type (might be due to invalid IL or missing references)
			//IL_068d: Expected O, but got Unknown
			//IL_068e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0698: Expected O, but got Unknown
			//IL_0699: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a3: Expected O, but got Unknown
			//IL_06a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ae: Expected O, but got Unknown
			//IL_06af: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b9: Expected O, but got Unknown
			//IL_06ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_06c4: Expected O, but got Unknown
			//IL_06c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06cf: Expected O, but got Unknown
			//IL_06d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06da: Expected O, but got Unknown
			//IL_06db: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e5: Expected O, but got Unknown
			//IL_06e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f0: Expected O, but got Unknown
			//IL_06f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06fb: Expected O, but got Unknown
			//IL_06fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0706: Expected O, but got Unknown
			//IL_0707: Unknown result type (might be due to invalid IL or missing references)
			//IL_0711: Expected O, but got Unknown
			//IL_0712: Unknown result type (might be due to invalid IL or missing references)
			//IL_071c: Expected O, but got Unknown
			//IL_071d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0727: Expected O, but got Unknown
			//IL_0728: Unknown result type (might be due to invalid IL or missing references)
			//IL_0732: Expected O, but got Unknown
			//IL_0733: Unknown result type (might be due to invalid IL or missing references)
			//IL_073d: Expected O, but got Unknown
			//IL_073e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0748: Expected O, but got Unknown
			//IL_0749: Unknown result type (might be due to invalid IL or missing references)
			//IL_0753: Expected O, but got Unknown
			//IL_0754: Unknown result type (might be due to invalid IL or missing references)
			//IL_075e: Expected O, but got Unknown
			//IL_075f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0769: Expected O, but got Unknown
			//IL_076a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0774: Expected O, but got Unknown
			//IL_0775: Unknown result type (might be due to invalid IL or missing references)
			//IL_077f: Expected O, but got Unknown
			//IL_0780: Unknown result type (might be due to invalid IL or missing references)
			//IL_078a: Expected O, but got Unknown
			//IL_078b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0795: Expected O, but got Unknown
			//IL_0796: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a0: Expected O, but got Unknown
			//IL_07a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ab: Expected O, but got Unknown
			//IL_07ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b6: Expected O, but got Unknown
			//IL_07b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c1: Expected O, but got Unknown
			//IL_07c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07cc: Expected O, but got Unknown
			//IL_07cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d7: Expected O, but got Unknown
			//IL_07d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e2: Expected O, but got Unknown
			//IL_07e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ed: Expected O, but got Unknown
			//IL_07ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f8: Expected O, but got Unknown
			//IL_07f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0803: Expected O, but got Unknown
			//IL_0804: Unknown result type (might be due to invalid IL or missing references)
			//IL_080e: Expected O, but got Unknown
			//IL_080f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0819: Expected O, but got Unknown
			//IL_081a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0824: Expected O, but got Unknown
			//IL_0825: Unknown result type (might be due to invalid IL or missing references)
			//IL_082f: Expected O, but got Unknown
			//IL_0830: Unknown result type (might be due to invalid IL or missing references)
			//IL_083a: Expected O, but got Unknown
			//IL_083b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0845: Expected O, but got Unknown
			//IL_0846: Unknown result type (might be due to invalid IL or missing references)
			//IL_0850: Expected O, but got Unknown
			//IL_0851: Unknown result type (might be due to invalid IL or missing references)
			//IL_085b: Expected O, but got Unknown
			//IL_085c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0866: Expected O, but got Unknown
			//IL_0867: Unknown result type (might be due to invalid IL or missing references)
			//IL_0871: Expected O, but got Unknown
			//IL_0872: Unknown result type (might be due to invalid IL or missing references)
			//IL_087c: Expected O, but got Unknown
			//IL_087d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0887: Expected O, but got Unknown
			//IL_0888: Unknown result type (might be due to invalid IL or missing references)
			//IL_0892: Expected O, but got Unknown
			//IL_0893: Unknown result type (might be due to invalid IL or missing references)
			//IL_089d: Expected O, but got Unknown
			//IL_089e: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a8: Expected O, but got Unknown
			//IL_08a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b3: Expected O, but got Unknown
			//IL_08b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08be: Expected O, but got Unknown
			//IL_08bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c9: Expected O, but got Unknown
			//IL_08ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_08d4: Expected O, but got Unknown
			//IL_08d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_08df: Expected O, but got Unknown
			//IL_08e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ea: Expected O, but got Unknown
			//IL_08eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_08f5: Expected O, but got Unknown
			//IL_08f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0900: Expected O, but got Unknown
			//IL_0901: Unknown result type (might be due to invalid IL or missing references)
			//IL_090b: Expected O, but got Unknown
			//IL_090c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0916: Expected O, but got Unknown
			//IL_0917: Unknown result type (might be due to invalid IL or missing references)
			//IL_0921: Expected O, but got Unknown
			//IL_0922: Unknown result type (might be due to invalid IL or missing references)
			//IL_092c: Expected O, but got Unknown
			//IL_092d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0937: Expected O, but got Unknown
			//IL_0938: Unknown result type (might be due to invalid IL or missing references)
			//IL_0942: Expected O, but got Unknown
			//IL_0943: Unknown result type (might be due to invalid IL or missing references)
			//IL_094d: Expected O, but got Unknown
			//IL_094e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0958: Expected O, but got Unknown
			//IL_0959: Unknown result type (might be due to invalid IL or missing references)
			//IL_0963: Expected O, but got Unknown
			//IL_0964: Unknown result type (might be due to invalid IL or missing references)
			//IL_096e: Expected O, but got Unknown
			//IL_096f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0979: Expected O, but got Unknown
			//IL_097a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0984: Expected O, but got Unknown
			//IL_0985: Unknown result type (might be due to invalid IL or missing references)
			//IL_098f: Expected O, but got Unknown
			//IL_0990: Unknown result type (might be due to invalid IL or missing references)
			//IL_099a: Expected O, but got Unknown
			//IL_099b: Unknown result type (might be due to invalid IL or missing references)
			//IL_09a5: Expected O, but got Unknown
			//IL_09a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_09b0: Expected O, but got Unknown
			//IL_09b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_09bb: Expected O, but got Unknown
			//IL_09bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_09c6: Expected O, but got Unknown
			//IL_09c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_09d1: Expected O, but got Unknown
			//IL_09d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_09dc: Expected O, but got Unknown
			//IL_09dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e7: Expected O, but got Unknown
			//IL_09e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_09f2: Expected O, but got Unknown
			//IL_09f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_09fd: Expected O, but got Unknown
			//IL_09fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a08: Expected O, but got Unknown
			//IL_0a09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a13: Expected O, but got Unknown
			//IL_0a14: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a1e: Expected O, but got Unknown
			//IL_0a1f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a29: Expected O, but got Unknown
			//IL_0a2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a34: Expected O, but got Unknown
			//IL_0a35: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a3f: Expected O, but got Unknown
			//IL_0a40: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a4a: Expected O, but got Unknown
			//IL_0a4b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a55: Expected O, but got Unknown
			//IL_0a56: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a60: Expected O, but got Unknown
			//IL_0a61: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a6b: Expected O, but got Unknown
			//IL_0a6c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a76: Expected O, but got Unknown
			//IL_0a77: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a81: Expected O, but got Unknown
			//IL_0a82: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a8c: Expected O, but got Unknown
			//IL_0a8d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a97: Expected O, but got Unknown
			//IL_0a98: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aa2: Expected O, but got Unknown
			//IL_0aa3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aad: Expected O, but got Unknown
			//IL_0aae: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab8: Expected O, but got Unknown
			//IL_0ab9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac3: Expected O, but got Unknown
			//IL_0ac4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ace: Expected O, but got Unknown
			//IL_0acf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ad9: Expected O, but got Unknown
			//IL_0ada: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ae4: Expected O, but got Unknown
			//IL_0ae5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aef: Expected O, but got Unknown
			//IL_0af0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0afa: Expected O, but got Unknown
			//IL_0afb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b05: Expected O, but got Unknown
			//IL_0b06: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b10: Expected O, but got Unknown
			//IL_0b11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b1b: Expected O, but got Unknown
			//IL_0b1c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b26: Expected O, but got Unknown
			//IL_0b27: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b31: Expected O, but got Unknown
			//IL_0b32: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b3c: Expected O, but got Unknown
			//IL_0b3d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b47: Expected O, but got Unknown
			//IL_0b48: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b52: Expected O, but got Unknown
			//IL_0b53: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b5d: Expected O, but got Unknown
			//IL_0b5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b68: Expected O, but got Unknown
			//IL_0b69: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b73: Expected O, but got Unknown
			//IL_0b74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b7e: Expected O, but got Unknown
			//IL_0b7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b89: Expected O, but got Unknown
			//IL_0b8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b94: Expected O, but got Unknown
			//IL_0b95: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b9f: Expected O, but got Unknown
			//IL_0ba0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0baa: Expected O, but got Unknown
			//IL_0bab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bb5: Expected O, but got Unknown
			//IL_0bb6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bc0: Expected O, but got Unknown
			//IL_0bc1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bcb: Expected O, but got Unknown
			//IL_0bcc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bd6: Expected O, but got Unknown
			//IL_0bd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0be1: Expected O, but got Unknown
			//IL_0be2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bec: Expected O, but got Unknown
			//IL_0bed: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bf7: Expected O, but got Unknown
			//IL_0bf8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c02: Expected O, but got Unknown
			//IL_0c03: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c0d: Expected O, but got Unknown
			//IL_0c0e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c18: Expected O, but got Unknown
			//IL_0c19: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c23: Expected O, but got Unknown
			//IL_0c24: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c2e: Expected O, but got Unknown
			//IL_0c2f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c39: Expected O, but got Unknown
			//IL_0c3a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c44: Expected O, but got Unknown
			//IL_0c45: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c4f: Expected O, but got Unknown
			//IL_0c50: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c5a: Expected O, but got Unknown
			//IL_0c5b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c65: Expected O, but got Unknown
			//IL_0c66: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c70: Expected O, but got Unknown
			//IL_0c71: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c7b: Expected O, but got Unknown
			//IL_0c7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c86: Expected O, but got Unknown
			//IL_0c87: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c91: Expected O, but got Unknown
			//IL_0c92: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c9c: Expected O, but got Unknown
			//IL_0c9d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ca7: Expected O, but got Unknown
			//IL_0cae: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cb8: Expected O, but got Unknown
			//IL_0cb9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cc3: Expected O, but got Unknown
			//IL_0cc4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cce: Expected O, but got Unknown
			//IL_0ccf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cd9: Expected O, but got Unknown
			//IL_0cda: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ce4: Expected O, but got Unknown
			//IL_0ce5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cef: Expected O, but got Unknown
			//IL_0cf0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cfa: Expected O, but got Unknown
			//IL_0cfb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d05: Expected O, but got Unknown
			//IL_1090: Unknown result type (might be due to invalid IL or missing references)
			//IL_109a: Expected O, but got Unknown
			//IL_110b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1115: Expected O, but got Unknown
			//IL_1122: Unknown result type (might be due to invalid IL or missing references)
			//IL_112c: Expected O, but got Unknown
			//IL_2590: Unknown result type (might be due to invalid IL or missing references)
			//IL_41e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_41ed: Expected O, but got Unknown
			//IL_4290: Unknown result type (might be due to invalid IL or missing references)
			//IL_429a: Expected O, but got Unknown
			//IL_433d: Unknown result type (might be due to invalid IL or missing references)
			//IL_4347: Expected O, but got Unknown
			//IL_43e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_43f3: Expected O, but got Unknown
			//IL_4a66: Unknown result type (might be due to invalid IL or missing references)
			//IL_4c8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_4c94: Expected O, but got Unknown
			//IL_4ca1: Unknown result type (might be due to invalid IL or missing references)
			//IL_4cab: Expected O, but got Unknown
			//IL_4cb8: Unknown result type (might be due to invalid IL or missing references)
			//IL_4cc2: Expected O, but got Unknown
			//IL_6a0c: Unknown result type (might be due to invalid IL or missing references)
			//IL_715b: Unknown result type (might be due to invalid IL or missing references)
			//IL_7165: Expected O, but got Unknown
			//IL_71e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_71eb: Expected O, but got Unknown
			//IL_76bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_76c9: Expected O, but got Unknown
			//IL_779b: Unknown result type (might be due to invalid IL or missing references)
			//IL_77a5: Expected O, but got Unknown
			//IL_7882: Unknown result type (might be due to invalid IL or missing references)
			//IL_788c: Expected O, but got Unknown
			//IL_7ebd: Unknown result type (might be due to invalid IL or missing references)
			//IL_7ec7: Expected O, but got Unknown
			//IL_84ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_84d8: Expected O, but got Unknown
			//IL_891f: Unknown result type (might be due to invalid IL or missing references)
			//IL_8929: Expected O, but got Unknown
			//IL_8cf3: Unknown result type (might be due to invalid IL or missing references)
			//IL_8cfd: Expected O, but got Unknown
			//IL_95a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_95b0: Expected O, but got Unknown
			//IL_9898: Unknown result type (might be due to invalid IL or missing references)
			//IL_98a2: Expected O, but got Unknown
			//IL_b902: Unknown result type (might be due to invalid IL or missing references)
			//IL_b90c: Expected O, but got Unknown
			//IL_bf61: Unknown result type (might be due to invalid IL or missing references)
			//IL_bf6b: Expected O, but got Unknown
			//IL_c655: Unknown result type (might be due to invalid IL or missing references)
			//IL_c65f: Expected O, but got Unknown
			//IL_c6c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_c6d3: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(PlotForm));
			panelPlot = new Panel();
			picPlot = new PictureBox();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			openCSVFileToolStripMenuItem = new ToolStripMenuItem();
			pasteCSVDataSetToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			correctForMissingExposuresToolStripMenuItem = new ToolStripMenuItem();
			plotUsingLargePointsToolStripMenuItem = new ToolStripMenuItem();
			toolTipToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			savePlotToolStripMenuItem = new ToolStripMenuItem();
			saveFormAsAnImageToolStripMenuItem = new ToolStripMenuItem();
			saveImageOfFormToolStripMenuItem = new ToolStripMenuItem();
			setScalecurrently100ToMatchMonitorScaleToolStripMenuItem = new ToolStripMenuItem();
			saveFourierToolStripMenuItem = new ToolStripMenuItem();
			alwaysShowComparisonsAtFileOpenToolStripMenuItem = new ToolStripMenuItem();
			editToolStripMenuItem = new ToolStripMenuItem();
			copyPlotToolStripMenuItem = new ToolStripMenuItem();
			copyFormToolStripMenuItem = new ToolStripMenuItem();
			copyFourierTransformOfLightCurveToolStripMenuItem = new ToolStripMenuItem();
			starDiameterAnalysisToolStripMenuItem = new ToolStripMenuItem();
			openStarDiameterAnalyserToolStripMenuItem = new ToolStripMenuItem();
			copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem = new ToolStripMenuItem();
			lightCurveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			hintsAndTipsToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem1 = new ToolStripMenuItem();
			withResultsToolStripMenuItem = new ToolStripMenuItem();
			cancelNoResultsToolStripMenuItem = new ToolStripMenuItem();
			chkComp1 = new CheckBox();
			chkComp2 = new CheckBox();
			txtBeforeD = new TextBox();
			txtAfterD = new TextBox();
			txtAfterR = new TextBox();
			txtBeforeR = new TextBox();
			optAfterD = new RadioButton();
			optBeforeR = new RadioButton();
			optAfterR = new RadioButton();
			optBeforeD = new RadioButton();
			label4 = new Label();
			label3 = new Label();
			txtSN_at_R = new TextBox();
			txtSN_at_D = new TextBox();
			tabPlot = new TabControl();
			tabPage1 = new TabPage();
			panelTab1 = new Panel();
			cmdCheckIntegration = new Button();
			cmdIntegrityCheck = new Button();
			cmdSetTimeScale = new Button();
			cmdChangeIntegration = new Button();
			cmdOpen = new Button();
			tabPage2 = new TabPage();
			panelTab6 = new Panel();
			picFourier = new PictureBox();
			tabPage3 = new TabPage();
			lblCopied = new Label();
			lblShortEventWarning = new Label();
			panel9 = new Panel();
			lblFrame = new Label();
			panel10 = new Panel();
			label73 = new Label();
			lblPossible = new Label();
			label74 = new Label();
			lblProbable = new Label();
			label78 = new Label();
			lblRatio12 = new Label();
			label72 = new Label();
			panel8 = new Panel();
			label75 = new Label();
			label71 = new Label();
			label70 = new Label();
			label68 = new Label();
			label67 = new Label();
			label47 = new Label();
			panel7 = new Panel();
			opt9 = new RadioButton();
			opt57 = new RadioButton();
			opt41 = new RadioButton();
			opt49 = new RadioButton();
			opt33 = new RadioButton();
			opt25 = new RadioButton();
			opt17 = new RadioButton();
			lblNoCompStars = new Label();
			lblTarget = new Label();
			picComp3 = new PictureBox();
			picComp2 = new PictureBox();
			picComp1 = new PictureBox();
			picTarget = new PictureBox();
			lstComp3 = new ListBox();
			lstComp2 = new ListBox();
			lstComp1 = new ListBox();
			lstTarget = new ListBox();
			optComp1 = new RadioButton();
			optComp2 = new RadioButton();
			optComp3 = new RadioButton();
			lblComp1 = new Label();
			lblComp2 = new Label();
			lblComp3 = new Label();
			lblHead2 = new Label();
			lblHead3 = new Label();
			lblHead1 = new Label();
			label69 = new Label();
			tabPage4 = new TabPage();
			panelTab2 = new Panel();
			panelFind = new Panel();
			cmdCancelFind = new Button();
			lblCurrentWidth = new Label();
			pBar = new ProgressBar();
			cmdFindPossible = new Button();
			grpMaxDurn = new GroupBox();
			txtMaxFrames = new TextBox();
			cmbIntegration = new ComboBox();
			label21 = new Label();
			optPAL = new RadioButton();
			optNTSC = new RadioButton();
			label15 = new Label();
			updnMaxDurn = new NumericUpDown();
			label14 = new Label();
			grpExpected = new GroupBox();
			txtMagDropLightLevel = new TextBox();
			label20 = new Label();
			label12 = new Label();
			updnMagStar = new NumericUpDown();
			label11 = new Label();
			label13 = new Label();
			updnMagObject = new NumericUpDown();
			updnMagDrop = new NumericUpDown();
			label10 = new Label();
			groupBox1 = new GroupBox();
			cmdSetLevelsAutomatic = new Button();
			label22 = new Label();
			txtExpectedSN = new TextBox();
			optPossOccLevel = new RadioButton();
			optFullLight = new RadioButton();
			txtFullPosn = new TextBox();
			txtFullLightLevel = new TextBox();
			txtOccLevelPos = new TextBox();
			txtOccultedLightLevel = new TextBox();
			label8 = new Label();
			label9 = new Label();
			label2 = new Label();
			tabPage5 = new TabPage();
			panelTab3 = new Panel();
			panelEvents = new Panel();
			panelExpectedMagDrop = new Panel();
			label66 = new Label();
			updnExpectedMagDrop = new NumericUpDown();
			label65 = new Label();
			label19 = new Label();
			cmdSetAllToMiss = new Button();
			label46 = new Label();
			label18 = new Label();
			label17 = new Label();
			label16 = new Label();
			tabPage6 = new TabPage();
			panelTab4 = new Panel();
			panelDR = new Panel();
			label25 = new Label();
			label24 = new Label();
			panelUncertainty = new Panel();
			label27 = new Label();
			picRUncert = new PictureBox();
			picDUncert = new PictureBox();
			panelTransitions = new Panel();
			label38 = new Label();
			label23 = new Label();
			label28 = new Label();
			picChiR = new PictureBox();
			picChiD = new PictureBox();
			label37 = new Label();
			panelGetLocations = new Panel();
			label64 = new Label();
			label63 = new Label();
			updnConfidenceR = new NumericUpDown();
			panel6 = new Panel();
			label62 = new Label();
			chkShowMeasurementMeans = new CheckBox();
			chkShowCrossCorr = new CheckBox();
			chkShowErrorBars = new CheckBox();
			label61 = new Label();
			updnConfidenceD = new NumericUpDown();
			label30 = new Label();
			lblDframe = new Label();
			panelSN = new Panel();
			label40 = new Label();
			lblRframe = new Label();
			cmdGetDandR = new Button();
			panelChi2 = new Panel();
			updnTransitionNumber = new NumericUpDown();
			panel3 = new Panel();
			label54 = new Label();
			optChi2StdDevn = new RadioButton();
			optChi2Variance = new RadioButton();
			label1 = new Label();
			label35 = new Label();
			label36 = new Label();
			label32 = new Label();
			panelMonteCarlo = new Panel();
			updnMonteCarlo = new NumericUpDown();
			panel4 = new Panel();
			optMonteCarloMeasuredSignal = new RadioButton();
			optMonteCarloTestSignal = new RadioButton();
			label49 = new Label();
			label34 = new Label();
			label33 = new Label();
			updnSDLimit = new NumericUpDown();
			label31 = new Label();
			panelAdjustRegions = new Panel();
			label29 = new Label();
			label26 = new Label();
			updnExtraPoints = new NumericUpDown();
			tabPage7 = new TabPage();
			panelTab5 = new Panel();
			lblEventIsMiss = new Label();
			panelView = new Panel();
			cmdReportLightCurve = new Button();
			lblEventsToAnalyse = new Label();
			label48 = new Label();
			cmdDisplayReport = new Button();
			cmdSaveReport = new Button();
			cmdSetMiss = new Button();
			grpSaveImages = new GroupBox();
			label80 = new Label();
			txtNameToAdd = new TextBox();
			panel12 = new Panel();
			lblMonitorScale = new Label();
			cmdSaveResults = new Button();
			cmdSaveMissAnalysis = new Button();
			chkAddNameToSaves = new CheckBox();
			grpFinalResults = new GroupBox();
			cmdVerifyTimes = new Button();
			lblSNR = new Label();
			lblDFrameFinal = new Label();
			lblRtransition = new Label();
			label53 = new Label();
			lblDtransition = new Label();
			label52 = new Label();
			label45 = new Label();
			label51 = new Label();
			label44 = new Label();
			label50 = new Label();
			label43 = new Label();
			lbl_UT_D = new Label();
			label6 = new Label();
			lbl_UT_R = new Label();
			lbl_UTuncert_D = new Label();
			lblRFrameFinal = new Label();
			lbl_UTuncert_R = new Label();
			lblTimeSource = new Label();
			label5 = new Label();
			grpCameraCorrections = new GroupBox();
			lblTangraCameraCorrns = new Label();
			panel5 = new Panel();
			lblSetNTSCorPAL = new Label();
			optSystemNotKnown = new RadioButton();
			optSystemOther = new RadioButton();
			label59 = new Label();
			optSystemNTSC = new RadioButton();
			optSystemPAL = new RadioButton();
			lblTimeDelaySecs = new Label();
			panel2 = new Panel();
			label56 = new Label();
			cmbCamera = new ComboBox();
			lblCameraDelayFrames = new Label();
			label42 = new Label();
			lblSelectCamera = new Label();
			panel1 = new Panel();
			lblIntegrationFrameDelay = new Label();
			label7 = new Label();
			cmbFrames = new ComboBox();
			label55 = new Label();
			lblSetIntegn = new Label();
			label58 = new Label();
			label39 = new Label();
			updnVerticalScaleAdjustment = new NumericUpDown();
			cmdTest = new Button();
			cmd_x10 = new Button();
			cmd_x15 = new Button();
			cmd_x5 = new Button();
			cmd_x1 = new Button();
			updnScale = new NumericUpDown();
			label41 = new Label();
			panelComp = new Panel();
			chkScaleComparisons = new CheckBox();
			cmbPlotAverage = new ComboBox();
			chkStarPoints = new CheckBox();
			label57 = new Label();
			chkComp3 = new CheckBox();
			chkTarget = new CheckBox();
			chkShowBackground = new CheckBox();
			toolTip = new ToolTip(components);
			lblRunningInTangra = new Label();
			label60 = new Label();
			pnlScale = new Panel();
			panel11 = new Panel();
			label77 = new Label();
			label76 = new Label();
			label79 = new Label();
			((Control)panelPlot).SuspendLayout();
			((ISupportInitialize)picPlot).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)tabPlot).SuspendLayout();
			((Control)tabPage1).SuspendLayout();
			((Control)panelTab1).SuspendLayout();
			((Control)tabPage2).SuspendLayout();
			((Control)panelTab6).SuspendLayout();
			((ISupportInitialize)picFourier).BeginInit();
			((Control)tabPage3).SuspendLayout();
			((Control)panel9).SuspendLayout();
			((Control)panel10).SuspendLayout();
			((Control)panel8).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((ISupportInitialize)picComp3).BeginInit();
			((ISupportInitialize)picComp2).BeginInit();
			((ISupportInitialize)picComp1).BeginInit();
			((ISupportInitialize)picTarget).BeginInit();
			((Control)tabPage4).SuspendLayout();
			((Control)panelTab2).SuspendLayout();
			((Control)panelFind).SuspendLayout();
			((Control)grpMaxDurn).SuspendLayout();
			((ISupportInitialize)updnMaxDurn).BeginInit();
			((Control)grpExpected).SuspendLayout();
			((ISupportInitialize)updnMagStar).BeginInit();
			((ISupportInitialize)updnMagObject).BeginInit();
			((ISupportInitialize)updnMagDrop).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((Control)tabPage5).SuspendLayout();
			((Control)panelTab3).SuspendLayout();
			((Control)panelEvents).SuspendLayout();
			((Control)panelExpectedMagDrop).SuspendLayout();
			((ISupportInitialize)updnExpectedMagDrop).BeginInit();
			((Control)tabPage6).SuspendLayout();
			((Control)panelTab4).SuspendLayout();
			((Control)panelDR).SuspendLayout();
			((Control)panelUncertainty).SuspendLayout();
			((ISupportInitialize)picRUncert).BeginInit();
			((ISupportInitialize)picDUncert).BeginInit();
			((Control)panelTransitions).SuspendLayout();
			((ISupportInitialize)picChiR).BeginInit();
			((ISupportInitialize)picChiD).BeginInit();
			((Control)panelGetLocations).SuspendLayout();
			((ISupportInitialize)updnConfidenceR).BeginInit();
			((Control)panel6).SuspendLayout();
			((ISupportInitialize)updnConfidenceD).BeginInit();
			((Control)panelSN).SuspendLayout();
			((Control)panelChi2).SuspendLayout();
			((ISupportInitialize)updnTransitionNumber).BeginInit();
			((Control)panel3).SuspendLayout();
			((Control)panelMonteCarlo).SuspendLayout();
			((ISupportInitialize)updnMonteCarlo).BeginInit();
			((Control)panel4).SuspendLayout();
			((ISupportInitialize)updnSDLimit).BeginInit();
			((Control)panelAdjustRegions).SuspendLayout();
			((ISupportInitialize)updnExtraPoints).BeginInit();
			((Control)tabPage7).SuspendLayout();
			((Control)panelTab5).SuspendLayout();
			((Control)panelView).SuspendLayout();
			((Control)grpSaveImages).SuspendLayout();
			((Control)panel12).SuspendLayout();
			((Control)grpFinalResults).SuspendLayout();
			((Control)grpCameraCorrections).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)updnVerticalScaleAdjustment).BeginInit();
			((ISupportInitialize)updnScale).BeginInit();
			((Control)panelComp).SuspendLayout();
			((Control)pnlScale).SuspendLayout();
			((Control)panel11).SuspendLayout();
			((Control)this).SuspendLayout();
			((ScrollableControl)panelPlot).set_AutoScroll(true);
			((Control)panelPlot).get_Controls().Add((Control)(object)picPlot);
			((Control)panelPlot).set_Location(new Point(4, 39));
			((Control)panelPlot).set_Name("panelPlot");
			((Control)panelPlot).set_Size(new Size(998, 291));
			((Control)panelPlot).set_TabIndex(3);
			((ScrollableControl)panelPlot).add_Scroll(new ScrollEventHandler(panelPlot_Scroll));
			((Control)picPlot).set_BackColor(Color.White);
			((Control)picPlot).set_Location(new Point(0, 0));
			((Control)picPlot).set_Name("picPlot");
			((Control)picPlot).set_Size(new Size(976, 252));
			picPlot.set_TabIndex(1);
			picPlot.set_TabStop(false);
			((Control)picPlot).add_MouseClick(new MouseEventHandler(picPlot_MouseClick));
			((Control)picPlot).add_MouseMove(new MouseEventHandler(picPlot_MouseMove));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)editToolStripMenuItem,
				(ToolStripItem)starDiameterAnalysisToolStripMenuItem,
				(ToolStripItem)lightCurveToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)hintsAndTipsToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem1
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1026, 28));
			((Control)menuStrip1).set_TabIndex(4);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[11]
			{
				(ToolStripItem)openCSVFileToolStripMenuItem,
				(ToolStripItem)pasteCSVDataSetToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)correctForMissingExposuresToolStripMenuItem,
				(ToolStripItem)plotUsingLargePointsToolStripMenuItem,
				(ToolStripItem)toolTipToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)savePlotToolStripMenuItem,
				(ToolStripItem)saveFormAsAnImageToolStripMenuItem,
				(ToolStripItem)saveFourierToolStripMenuItem,
				(ToolStripItem)alwaysShowComparisonsAtFileOpenToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(55, 24));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...   ");
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Image((Image)Resources.OpenSelectedItemHS);
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Name("openCSVFileToolStripMenuItem");
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Text("Open CSV file");
			((ToolStripItem)openCSVFileToolStripMenuItem).add_Click((EventHandler)openCSVFileToolStripMenuItem_Click);
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Image((Image)Resources.PasteHS);
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Name("pasteCSVDataSetToolStripMenuItem");
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Text("Paste CSV data set");
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).add_Click((EventHandler)pasteCSVDataSetToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(316, 6));
			correctForMissingExposuresToolStripMenuItem.set_Checked(true);
			correctForMissingExposuresToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).set_Name("correctForMissingExposuresToolStripMenuItem");
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).set_Text("Correct for missing exposures");
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).add_Click((EventHandler)correctForMissingExposuresToolStripMenuItem_Click);
			plotUsingLargePointsToolStripMenuItem.set_Checked(Settings.Default.AOTA_PlotUsingLarge);
			plotUsingLargePointsToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)plotUsingLargePointsToolStripMenuItem).set_Name("plotUsingLargePointsToolStripMenuItem");
			((ToolStripItem)plotUsingLargePointsToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)plotUsingLargePointsToolStripMenuItem).set_Text("Plot using large points");
			((ToolStripItem)plotUsingLargePointsToolStripMenuItem).add_Click((EventHandler)plotUsingLargePointsToolStripMenuItem_Click);
			((ToolStripItem)toolTipToolStripMenuItem).set_Image((Image)Resources.CLOCK02);
			((ToolStripItem)toolTipToolStripMenuItem).set_Name("toolTipToolStripMenuItem");
			((ToolStripItem)toolTipToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)toolTipToolStripMenuItem).set_Text("Show frame && time inToolTip");
			((ToolStripItem)toolTipToolStripMenuItem).add_Click((EventHandler)toolTipToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(316, 6));
			((ToolStripItem)savePlotToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)savePlotToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)savePlotToolStripMenuItem).set_Name("savePlotToolStripMenuItem");
			((ToolStripItem)savePlotToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)savePlotToolStripMenuItem).set_Text("Save plot");
			((ToolStripItem)savePlotToolStripMenuItem).add_Click((EventHandler)savePlotToolStripMenuItem_Click);
			((ToolStripDropDownItem)saveFormAsAnImageToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)saveImageOfFormToolStripMenuItem,
				(ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem
			});
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Image((Image)Resources.SaveAsWebPageHS);
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_ImageTransparentColor(Color.Black);
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Name("saveFormAsAnImageToolStripMenuItem");
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Text("Save image of this form as currently displayed");
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).add_Click((EventHandler)saveFormAsAnImageToolStripMenuItem_Click);
			((ToolStripItem)saveImageOfFormToolStripMenuItem).set_Name("saveImageOfFormToolStripMenuItem");
			((ToolStripItem)saveImageOfFormToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)saveImageOfFormToolStripMenuItem).set_Text("Save image of form");
			((ToolStripItem)saveImageOfFormToolStripMenuItem).add_Click((EventHandler)saveImageOfFormToolStripMenuItem_Click);
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Name("setScalecurrently100ToMatchMonitorScaleToolStripMenuItem");
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Text("Set scale (currently 100%) to match Monitor scale");
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).add_Click((EventHandler)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem_Click);
			((ToolStripItem)saveFourierToolStripMenuItem).set_Image((Image)Resources.FunctionHS);
			((ToolStripItem)saveFourierToolStripMenuItem).set_Name("saveFourierToolStripMenuItem");
			((ToolStripItem)saveFourierToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)saveFourierToolStripMenuItem).set_Text("Save Fourier transform of light curve");
			((ToolStripItem)saveFourierToolStripMenuItem).add_Click((EventHandler)saveFourierToolStripMenuItem_Click);
			alwaysShowComparisonsAtFileOpenToolStripMenuItem.set_Checked(Settings.Default.AOTAshowComparisons);
			alwaysShowComparisonsAtFileOpenToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)alwaysShowComparisonsAtFileOpenToolStripMenuItem).set_Name("alwaysShowComparisonsAtFileOpenToolStripMenuItem");
			((ToolStripItem)alwaysShowComparisonsAtFileOpenToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)alwaysShowComparisonsAtFileOpenToolStripMenuItem).set_Text("Always show comparisons stars with File Open");
			((ToolStripItem)alwaysShowComparisonsAtFileOpenToolStripMenuItem).add_Click((EventHandler)alewaysShowComparisonsAtFileOpenToolStripMenuItem_Click);
			((ToolStripDropDownItem)editToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyPlotToolStripMenuItem,
				(ToolStripItem)copyFormToolStripMenuItem,
				(ToolStripItem)copyFourierTransformOfLightCurveToolStripMenuItem
			});
			((ToolStripItem)editToolStripMenuItem).set_Name("editToolStripMenuItem");
			((ToolStripItem)editToolStripMenuItem).set_Size(new Size(60, 24));
			((ToolStripItem)editToolStripMenuItem).set_Text("Edit...    ");
			((ToolStripItem)copyPlotToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyPlotToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyPlotToolStripMenuItem).set_Name("copyPlotToolStripMenuItem");
			((ToolStripItem)copyPlotToolStripMenuItem).set_Size(new Size(270, 22));
			((ToolStripItem)copyPlotToolStripMenuItem).set_Text("Copy plot");
			((ToolStripItem)copyPlotToolStripMenuItem).add_Click((EventHandler)copyPlotToolStripMenuItem_Click);
			((ToolStripItem)copyFormToolStripMenuItem).set_ImageTransparentColor(Color.Black);
			((ToolStripItem)copyFormToolStripMenuItem).set_Name("copyFormToolStripMenuItem");
			((ToolStripItem)copyFormToolStripMenuItem).set_Size(new Size(270, 22));
			((ToolStripItem)copyFormToolStripMenuItem).set_Text("Copy form");
			((ToolStripItem)copyFormToolStripMenuItem).add_Click((EventHandler)copyFormToolStripMenuItem_Click);
			((ToolStripItem)copyFourierTransformOfLightCurveToolStripMenuItem).set_Name("copyFourierTransformOfLightCurveToolStripMenuItem");
			((ToolStripItem)copyFourierTransformOfLightCurveToolStripMenuItem).set_Size(new Size(270, 22));
			((ToolStripItem)copyFourierTransformOfLightCurveToolStripMenuItem).set_Text("Copy Fourier transform of light curve");
			((ToolStripItem)copyFourierTransformOfLightCurveToolStripMenuItem).add_Click((EventHandler)copyFourierTransformOfLightCurveToolStripMenuItem_Click);
			((ToolStripDropDownItem)starDiameterAnalysisToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)openStarDiameterAnalyserToolStripMenuItem,
				(ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem
			});
			((ToolStripItem)starDiameterAnalysisToolStripMenuItem).set_Name("starDiameterAnalysisToolStripMenuItem");
			((ToolStripItem)starDiameterAnalysisToolStripMenuItem).set_Size(new Size(151, 24));
			((ToolStripItem)starDiameterAnalysisToolStripMenuItem).set_Text("Star diameter analysis...   ");
			openStarDiameterAnalyserToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)openStarDiameterAnalyserToolStripMenuItem).set_Name("openStarDiameterAnalyserToolStripMenuItem");
			((ToolStripItem)openStarDiameterAnalyserToolStripMenuItem).set_Size(new Size(321, 22));
			((ToolStripItem)openStarDiameterAnalyserToolStripMenuItem).set_Text("Show star diameter analyser");
			((ToolStripItem)openStarDiameterAnalyserToolStripMenuItem).add_Click((EventHandler)openStarDiameterAnalyserToolStripMenuItem_Click);
			((ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem).set_Name("copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem");
			((ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem).set_Size(new Size(321, 22));
			((ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem).set_Text("Copy current measurement regions to analyser");
			((ToolStripItem)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem).add_Click((EventHandler)copyCurrentMeasurementRegionsToAnalyserToolStripMenuItem_Click);
			((ToolStripItem)lightCurveToolStripMenuItem).set_Name("lightCurveToolStripMenuItem");
			((ToolStripItem)lightCurveToolStripMenuItem).set_Size(new Size(113, 24));
			((ToolStripItem)lightCurveToolStripMenuItem).set_Text("Light curve report");
			((ToolStripItem)lightCurveToolStripMenuItem).add_Click((EventHandler)lightCurveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 24));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help     ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)hintsAndTipsToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)hintsAndTipsToolStripMenuItem).set_Name("hintsAndTipsToolStripMenuItem");
			((ToolStripItem)hintsAndTipsToolStripMenuItem).set_Size(new Size(119, 24));
			((ToolStripItem)hintsAndTipsToolStripMenuItem).set_Text("Hints and Tips   ");
			((ToolStripItem)hintsAndTipsToolStripMenuItem).add_Click((EventHandler)hintsAndTipsToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Font(new Font("Segoe UI", 11f, FontStyle.Bold));
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(83, 24));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit     ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripDropDownItem)exitToolStripMenuItem1).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)withResultsToolStripMenuItem,
				(ToolStripItem)cancelNoResultsToolStripMenuItem
			});
			((ToolStripItem)exitToolStripMenuItem1).set_Font(new Font("Segoe UI", 11f, FontStyle.Bold));
			((ToolStripItem)exitToolStripMenuItem1).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem1).set_Name("exitToolStripMenuItem1");
			((ToolStripItem)exitToolStripMenuItem1).set_Size(new Size(79, 24));
			((ToolStripItem)exitToolStripMenuItem1).set_Text("Exit ...");
			((ToolStripItem)withResultsToolStripMenuItem).set_Image((Image)Resources.check);
			((ToolStripItem)withResultsToolStripMenuItem).set_Name("withResultsToolStripMenuItem");
			((ToolStripItem)withResultsToolStripMenuItem).set_Size(new Size(257, 24));
			((ToolStripItem)withResultsToolStripMenuItem).set_Text("with results");
			((ToolStripItem)withResultsToolStripMenuItem).add_Click((EventHandler)withResultsToolStripMenuItem_Click);
			((ToolStripItem)cancelNoResultsToolStripMenuItem).set_Image((Image)Resources.cancel);
			((ToolStripItem)cancelNoResultsToolStripMenuItem).set_Name("cancelNoResultsToolStripMenuItem");
			((ToolStripItem)cancelNoResultsToolStripMenuItem).set_Size(new Size(257, 24));
			((ToolStripItem)cancelNoResultsToolStripMenuItem).set_Text("cancel analysis, no results");
			((ToolStripItem)cancelNoResultsToolStripMenuItem).add_Click((EventHandler)cancelNoResultsToolStripMenuItem_Click);
			((Control)chkComp1).set_Anchor((AnchorStyles)10);
			((Control)chkComp1).set_AutoSize(true);
			((Control)chkComp1).set_BackColor(Color.LimeGreen);
			chkComp1.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkComp1).set_ForeColor(Color.Black);
			((Control)chkComp1).set_Location(new Point(6, 39));
			((Control)chkComp1).set_Name("chkComp1");
			((Control)chkComp1).set_Size(new Size(110, 17));
			((Control)chkComp1).set_TabIndex(8);
			((Control)chkComp1).set_Text("Comparison star 1");
			((ButtonBase)chkComp1).set_UseVisualStyleBackColor(false);
			chkComp1.add_CheckedChanged((EventHandler)chkComp1_CheckedChanged);
			((Control)chkComp2).set_Anchor((AnchorStyles)10);
			((Control)chkComp2).set_AutoSize(true);
			((Control)chkComp2).set_BackColor(Color.Orange);
			chkComp2.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkComp2).set_ForeColor(Color.Black);
			((Control)chkComp2).set_Location(new Point(6, 54));
			((Control)chkComp2).set_Name("chkComp2");
			((Control)chkComp2).set_Size(new Size(110, 17));
			((Control)chkComp2).set_TabIndex(9);
			((Control)chkComp2).set_Text("Comparison star 2");
			((ButtonBase)chkComp2).set_UseVisualStyleBackColor(false);
			chkComp2.add_CheckedChanged((EventHandler)chkComp1_CheckedChanged);
			((TextBoxBase)txtBeforeD).set_BorderStyle((BorderStyle)1);
			((Control)txtBeforeD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBeforeD).set_Location(new Point(7, 53));
			((Control)txtBeforeD).set_Name("txtBeforeD");
			((TextBoxBase)txtBeforeD).set_ReadOnly(true);
			((Control)txtBeforeD).set_Size(new Size(39, 20));
			((Control)txtBeforeD).set_TabIndex(17);
			((Control)txtBeforeD).set_Text("0");
			((TextBoxBase)txtAfterD).set_BorderStyle((BorderStyle)1);
			((Control)txtAfterD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAfterD).set_Location(new Point(7, 79));
			((Control)txtAfterD).set_Name("txtAfterD");
			((TextBoxBase)txtAfterD).set_ReadOnly(true);
			((Control)txtAfterD).set_Size(new Size(39, 20));
			((Control)txtAfterD).set_TabIndex(16);
			((Control)txtAfterD).set_Text("0");
			((TextBoxBase)txtAfterR).set_BorderStyle((BorderStyle)1);
			((Control)txtAfterR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAfterR).set_Location(new Point(7, 131));
			((Control)txtAfterR).set_Name("txtAfterR");
			((TextBoxBase)txtAfterR).set_ReadOnly(true);
			((Control)txtAfterR).set_Size(new Size(39, 20));
			((Control)txtAfterR).set_TabIndex(15);
			((Control)txtAfterR).set_Text("0");
			((TextBoxBase)txtBeforeR).set_BorderStyle((BorderStyle)1);
			((Control)txtBeforeR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBeforeR).set_Location(new Point(7, 105));
			((Control)txtBeforeR).set_Name("txtBeforeR");
			((TextBoxBase)txtBeforeR).set_ReadOnly(true);
			((Control)txtBeforeR).set_Size(new Size(39, 20));
			((Control)txtBeforeR).set_TabIndex(14);
			((Control)txtBeforeR).set_Text("0");
			((Control)optAfterD).set_AutoSize(true);
			((Control)optAfterD).set_Location(new Point(52, 81));
			((Control)optAfterD).set_Name("optAfterD");
			((Control)optAfterD).set_Size(new Size(58, 17));
			((Control)optAfterD).set_TabIndex(3);
			((Control)optAfterD).set_Text("After D");
			((ButtonBase)optAfterD).set_UseVisualStyleBackColor(true);
			((Control)optBeforeR).set_AutoSize(true);
			((Control)optBeforeR).set_Location(new Point(52, 107));
			((Control)optBeforeR).set_Name("optBeforeR");
			((Control)optBeforeR).set_Size(new Size(67, 17));
			((Control)optBeforeR).set_TabIndex(2);
			((Control)optBeforeR).set_Text("Before R");
			((ButtonBase)optBeforeR).set_UseVisualStyleBackColor(true);
			((Control)optAfterR).set_AutoSize(true);
			((Control)optAfterR).set_Location(new Point(52, 133));
			((Control)optAfterR).set_Name("optAfterR");
			((Control)optAfterR).set_Size(new Size(58, 17));
			((Control)optAfterR).set_TabIndex(1);
			((Control)optAfterR).set_Text("After R");
			((ButtonBase)optAfterR).set_UseVisualStyleBackColor(true);
			((Control)optBeforeD).set_AutoSize(true);
			optBeforeD.set_Checked(true);
			((Control)optBeforeD).set_Location(new Point(52, 55));
			((Control)optBeforeD).set_Name("optBeforeD");
			((Control)optBeforeD).set_Size(new Size(67, 17));
			((Control)optBeforeD).set_TabIndex(0);
			optBeforeD.set_TabStop(true);
			((Control)optBeforeD).set_Text("Before D");
			((ButtonBase)optBeforeD).set_UseVisualStyleBackColor(true);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(4, 4));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(27, 13));
			((Control)label4).set_TabIndex(25);
			((Control)label4).set_Text("S/N");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(35, 4));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(31, 13));
			((Control)label3).set_TabIndex(24);
			((Control)label3).set_Text("at D");
			((TextBoxBase)txtSN_at_R).set_BorderStyle((BorderStyle)1);
			((Control)txtSN_at_R).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtSN_at_R).set_Location(new Point(142, 1));
			((Control)txtSN_at_R).set_Name("txtSN_at_R");
			((TextBoxBase)txtSN_at_R).set_ReadOnly(true);
			((Control)txtSN_at_R).set_Size(new Size(31, 19));
			((Control)txtSN_at_R).set_TabIndex(20);
			((Control)txtSN_at_R).set_Text("0");
			((TextBoxBase)txtSN_at_D).set_BorderStyle((BorderStyle)1);
			((Control)txtSN_at_D).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtSN_at_D).set_Location(new Point(68, 1));
			((Control)txtSN_at_D).set_Name("txtSN_at_D");
			((TextBoxBase)txtSN_at_D).set_ReadOnly(true);
			((Control)txtSN_at_D).set_Size(new Size(31, 19));
			((Control)txtSN_at_D).set_TabIndex(18);
			((Control)txtSN_at_D).set_Text("0");
			((Control)tabPlot).get_Controls().Add((Control)(object)tabPage1);
			((Control)tabPlot).get_Controls().Add((Control)(object)tabPage2);
			((Control)tabPlot).get_Controls().Add((Control)(object)tabPage3);
			((Control)tabPlot).get_Controls().Add((Control)(object)tabPage4);
			((Control)tabPlot).get_Controls().Add((Control)(object)tabPage5);
			((Control)tabPlot).get_Controls().Add((Control)(object)tabPage6);
			((Control)tabPlot).get_Controls().Add((Control)(object)tabPage7);
			((Control)tabPlot).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)tabPlot).set_Location(new Point(124, 336));
			((Control)tabPlot).set_Name("tabPlot");
			tabPlot.set_SelectedIndex(0);
			((Control)tabPlot).set_Size(new Size(900, 192));
			((Control)tabPlot).set_TabIndex(12);
			tabPlot.add_SelectedIndexChanged((EventHandler)tabPlot_SelectedIndexChanged);
			((Control)tabPage1).set_BackColor(Color.LemonChiffon);
			((Panel)tabPage1).set_BorderStyle((BorderStyle)2);
			((Control)tabPage1).get_Controls().Add((Control)(object)panelTab1);
			tabPage1.set_Location(new Point(4, 22));
			((Control)tabPage1).set_Name("tabPage1");
			((Control)tabPage1).set_Padding(new Padding(3));
			((Control)tabPage1).set_Size(new Size(892, 166));
			tabPage1.set_TabIndex(0);
			((Control)tabPage1).set_Text("1.  Read, Integrity, Set time, Bin && normalise   ");
			tabPage1.set_UseVisualStyleBackColor(true);
			((Control)panelTab1).get_Controls().Add((Control)(object)cmdCheckIntegration);
			((Control)panelTab1).get_Controls().Add((Control)(object)cmdIntegrityCheck);
			((Control)panelTab1).get_Controls().Add((Control)(object)cmdSetTimeScale);
			((Control)panelTab1).get_Controls().Add((Control)(object)cmdChangeIntegration);
			((Control)panelTab1).get_Controls().Add((Control)(object)cmdOpen);
			((Control)panelTab1).set_Location(new Point(1, 0));
			((Control)panelTab1).set_Name("panelTab1");
			((Control)panelTab1).set_Size(new Size(881, 158));
			((Control)panelTab1).set_TabIndex(11);
			((Control)cmdCheckIntegration).set_Location(new Point(601, 112));
			((Control)cmdCheckIntegration).set_Name("cmdCheckIntegration");
			((Control)cmdCheckIntegration).set_Size(new Size(115, 32));
			((Control)cmdCheckIntegration).set_TabIndex(14);
			((Control)cmdCheckIntegration).set_Text("Check integration");
			((ButtonBase)cmdCheckIntegration).set_UseVisualStyleBackColor(true);
			((Control)cmdCheckIntegration).add_Click((EventHandler)cmdCheckIntegration_Click);
			((Control)cmdIntegrityCheck).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdIntegrityCheck).set_Location(new Point(210, 49));
			((Control)cmdIntegrityCheck).set_Name("cmdIntegrityCheck");
			((Control)cmdIntegrityCheck).set_Size(new Size(149, 43));
			((Control)cmdIntegrityCheck).set_TabIndex(13);
			((Control)cmdIntegrityCheck).set_Text("Integrity check");
			((ButtonBase)cmdIntegrityCheck).set_UseVisualStyleBackColor(true);
			((Control)cmdIntegrityCheck).add_Click((EventHandler)cmdIntegrityCheck_Click);
			((Control)cmdSetTimeScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSetTimeScale).set_Location(new Point(407, 49));
			((Control)cmdSetTimeScale).set_Name("cmdSetTimeScale");
			((Control)cmdSetTimeScale).set_Size(new Size(149, 43));
			((Control)cmdSetTimeScale).set_TabIndex(12);
			((Control)cmdSetTimeScale).set_Text("Set or Edit\r\nthe time scale");
			((ButtonBase)cmdSetTimeScale).set_UseVisualStyleBackColor(true);
			((Control)cmdSetTimeScale).add_Click((EventHandler)cmdSetTimeScale_Click);
			((Control)cmdChangeIntegration).set_Enabled(false);
			((Control)cmdChangeIntegration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdChangeIntegration).set_Location(new Point(604, 40));
			((Control)cmdChangeIntegration).set_Name("cmdChangeIntegration");
			((Control)cmdChangeIntegration).set_Size(new Size(149, 60));
			((Control)cmdChangeIntegration).set_TabIndex(10);
			((Control)cmdChangeIntegration).set_Text("* Background\r\n* Integration    \r\n* Binning \r\n* Normalisation");
			((ButtonBase)cmdChangeIntegration).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdChangeIntegration).set_UseVisualStyleBackColor(true);
			((Control)cmdChangeIntegration).add_Click((EventHandler)cmdChangeIntegration_Click);
			((Control)cmdOpen).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdOpen).set_Location(new Point(13, 49));
			((Control)cmdOpen).set_Name("cmdOpen");
			((Control)cmdOpen).set_Size(new Size(149, 43));
			((Control)cmdOpen).set_TabIndex(11);
			((Control)cmdOpen).set_Text("Open a CSV file");
			((ButtonBase)cmdOpen).set_UseVisualStyleBackColor(true);
			((Control)cmdOpen).add_Click((EventHandler)cmdOpen_Click);
			((Panel)tabPage2).set_BorderStyle((BorderStyle)2);
			((Control)tabPage2).get_Controls().Add((Control)(object)panelTab6);
			tabPage2.set_Location(new Point(4, 22));
			((Control)tabPage2).set_Name("tabPage2");
			((Control)tabPage2).set_Size(new Size(892, 166));
			tabPage2.set_TabIndex(5);
			((Control)tabPage2).set_Text("2A. Fourier plot");
			tabPage2.set_UseVisualStyleBackColor(true);
			((Control)panelTab6).set_BackColor(Color.PeachPuff);
			((Control)panelTab6).get_Controls().Add((Control)(object)picFourier);
			((Control)panelTab6).set_Location(new Point(2, 2));
			((Control)panelTab6).set_Name("panelTab6");
			((Control)panelTab6).set_Size(new Size(889, 161));
			((Control)panelTab6).set_TabIndex(0);
			((Control)picFourier).set_BackColor(Color.White);
			((Control)picFourier).set_Location(new Point(2, 2));
			((Control)picFourier).set_Name("picFourier");
			((Control)picFourier).set_Size(new Size(884, 157));
			picFourier.set_TabIndex(0);
			picFourier.set_TabStop(false);
			((Control)tabPage3).set_BackColor(Color.SeaShell);
			((Panel)tabPage3).set_BorderStyle((BorderStyle)2);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblCopied);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblShortEventWarning);
			((Control)tabPage3).get_Controls().Add((Control)(object)panel9);
			((Control)tabPage3).get_Controls().Add((Control)(object)label72);
			((Control)tabPage3).get_Controls().Add((Control)(object)panel8);
			((Control)tabPage3).get_Controls().Add((Control)(object)label47);
			((Control)tabPage3).get_Controls().Add((Control)(object)panel7);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblNoCompStars);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblTarget);
			((Control)tabPage3).get_Controls().Add((Control)(object)picComp3);
			((Control)tabPage3).get_Controls().Add((Control)(object)picComp2);
			((Control)tabPage3).get_Controls().Add((Control)(object)picComp1);
			((Control)tabPage3).get_Controls().Add((Control)(object)picTarget);
			((Control)tabPage3).get_Controls().Add((Control)(object)lstComp3);
			((Control)tabPage3).get_Controls().Add((Control)(object)lstComp2);
			((Control)tabPage3).get_Controls().Add((Control)(object)lstComp1);
			((Control)tabPage3).get_Controls().Add((Control)(object)lstTarget);
			((Control)tabPage3).get_Controls().Add((Control)(object)optComp3);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblComp1);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblComp2);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblComp3);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblHead2);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblHead3);
			((Control)tabPage3).get_Controls().Add((Control)(object)lblHead1);
			((Control)tabPage3).get_Controls().Add((Control)(object)label69);
			((Control)tabPage3).get_Controls().Add((Control)(object)optComp1);
			((Control)tabPage3).get_Controls().Add((Control)(object)optComp2);
			((Control)tabPage3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			tabPage3.set_Location(new Point(4, 22));
			((Control)tabPage3).set_Name("tabPage3");
			((Control)tabPage3).set_Size(new Size(892, 166));
			tabPage3.set_TabIndex(6);
			((Control)tabPage3).set_Text("2B. [1, 2 && 3] - frame test  ");
			((Control)lblCopied).set_AutoSize(true);
			((Control)lblCopied).set_BackColor(Color.Fuchsia);
			((Control)lblCopied).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCopied).set_ForeColor(Color.Yellow);
			((Control)lblCopied).set_Location(new Point(236, 132));
			((Control)lblCopied).set_Name("lblCopied");
			((Control)lblCopied).set_Size(new Size(101, 20));
			((Control)lblCopied).set_TabIndex(72);
			((Control)lblCopied).set_Text("Text copied");
			((Control)lblCopied).set_Visible(false);
			((Control)lblShortEventWarning).set_AutoSize(true);
			((Control)lblShortEventWarning).set_BackColor(Color.RoyalBlue);
			lblShortEventWarning.set_BorderStyle((BorderStyle)2);
			((Control)lblShortEventWarning).set_Font(new Font("Microsoft Sans Serif", 28f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblShortEventWarning).set_ForeColor(Color.Yellow);
			((Control)lblShortEventWarning).set_Location(new Point(167, 58));
			((Control)lblShortEventWarning).set_Name("lblShortEventWarning");
			((Control)lblShortEventWarning).set_Size(new Size(473, 46));
			((Control)lblShortEventWarning).set_TabIndex(71);
			((Control)lblShortEventWarning).set_Text("For 1, 2 && 3 frame events");
			((Control)panel9).set_BackColor(Color.FromArgb(255, 255, 192));
			panel9.set_BorderStyle((BorderStyle)2);
			((Control)panel9).get_Controls().Add((Control)(object)lblFrame);
			((Control)panel9).get_Controls().Add((Control)(object)panel10);
			((Control)panel9).get_Controls().Add((Control)(object)lblRatio12);
			((Control)panel9).set_Location(new Point(4, 29));
			((Control)panel9).set_Name("panel9");
			((Control)panel9).set_Size(new Size(133, 47));
			((Control)panel9).set_TabIndex(69);
			((Control)lblFrame).set_AutoSize(true);
			((Control)lblFrame).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFrame).set_Location(new Point(80, 28));
			((Control)lblFrame).set_Name("lblFrame");
			((Control)lblFrame).set_Size(new Size(19, 13));
			((Control)lblFrame).set_TabIndex(30);
			((Control)lblFrame).set_Text("#0");
			((Control)panel10).set_BackColor(Color.Azure);
			((Control)panel10).get_Controls().Add((Control)(object)label73);
			((Control)panel10).get_Controls().Add((Control)(object)lblPossible);
			((Control)panel10).get_Controls().Add((Control)(object)label74);
			((Control)panel10).get_Controls().Add((Control)(object)lblProbable);
			((Control)panel10).get_Controls().Add((Control)(object)label78);
			((Control)panel10).set_Location(new Point(-1, -2));
			((Control)panel10).set_Name("panel10");
			((Control)panel10).set_Size(new Size(78, 47));
			((Control)panel10).set_TabIndex(29);
			((Control)label73).set_AutoSize(true);
			((Control)label73).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label73).set_ForeColor(Color.Purple);
			((Control)label73).set_Location(new Point(0, 1));
			((Control)label73).set_Name("label73");
			((Control)label73).set_Size(new Size(73, 13));
			((Control)label73).set_TabIndex(71);
			((Control)label73).set_Text("Scaled values");
			((Control)lblPossible).set_AutoSize(true);
			((Control)lblPossible).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPossible).set_ForeColor(Color.Navy);
			((Control)lblPossible).set_Location(new Point(43, 30));
			((Control)lblPossible).set_Name("lblPossible");
			((Control)lblPossible).set_Size(new Size(34, 13));
			((Control)lblPossible).set_TabIndex(27);
			((Control)lblPossible).set_Text(">1.25");
			((Control)label74).set_AutoSize(true);
			((Control)label74).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label74).set_Location(new Point(-1, 30));
			((Control)label74).set_Name("label74");
			((Control)label74).set_Size(new Size(46, 13));
			((Control)label74).set_TabIndex(25);
			((Control)label74).set_Text("Possible");
			((Control)lblProbable).set_AutoSize(true);
			((Control)lblProbable).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblProbable).set_ForeColor(Color.DarkRed);
			((Control)lblProbable).set_Location(new Point(43, 16));
			((Control)lblProbable).set_Name("lblProbable");
			((Control)lblProbable).set_Size(new Size(34, 13));
			((Control)lblProbable).set_TabIndex(26);
			((Control)lblProbable).set_Text(">2.00");
			((Control)label78).set_AutoSize(true);
			((Control)label78).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label78).set_Location(new Point(-1, 16));
			((Control)label78).set_Name("label78");
			((Control)label78).set_Size(new Size(49, 13));
			((Control)label78).set_TabIndex(24);
			((Control)label78).set_Text("Probable");
			((Control)lblRatio12).set_AutoSize(true);
			((Control)lblRatio12).set_Font(new Font("Arial", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblRatio12).set_ForeColor(Color.FromArgb(210, 30, 0));
			((Control)lblRatio12).set_Location(new Point(77, 6));
			((Control)lblRatio12).set_Name("lblRatio12");
			((Control)lblRatio12).set_Size(new Size(32, 16));
			((Control)lblRatio12).set_TabIndex(21);
			((Control)lblRatio12).set_Text("1.00");
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label72).set_ForeColor(Color.FromArgb(210, 30, 0));
			((Control)label72).set_Location(new Point(53, 13));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(84, 17));
			((Control)label72).set_TabIndex(24);
			((Control)label72).set_Text("Evaluation");
			toolTip.SetToolTip((Control)(object)label72, componentResourceManager.GetString("label72.ToolTip"));
			panel8.set_BorderStyle((BorderStyle)2);
			((Control)panel8).get_Controls().Add((Control)(object)label75);
			((Control)panel8).get_Controls().Add((Control)(object)label71);
			((Control)panel8).get_Controls().Add((Control)(object)label70);
			((Control)panel8).get_Controls().Add((Control)(object)label68);
			((Control)panel8).get_Controls().Add((Control)(object)label67);
			((Control)panel8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panel8).set_Location(new Point(710, 3));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(175, 157));
			((Control)panel8).set_TabIndex(16);
			((Control)label75).set_AutoSize(true);
			((Control)label75).set_ForeColor(Color.MediumBlue);
			((Control)label75).set_Location(new Point(2, 64));
			((Control)label75).set_Name("label75");
			((Control)label75).set_Size(new Size(170, 26));
			((Control)label75).set_TabIndex(4);
			((Control)label75).set_Text("Value varies according to the\r\n# data points && # comparison stars");
			((Control)label71).set_AutoSize(true);
			((Control)label71).set_ForeColor(Color.DarkRed);
			((Control)label71).set_Location(new Point(2, 128));
			((Control)label71).set_Name("label71");
			((Control)label71).set_Size(new Size(155, 26));
			((Control)label71).set_TabIndex(3);
			((Control)label71).set_Text("Comparisons in light red may be\r\nunsuitable");
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_ForeColor(Color.FromArgb(0, 70, 0));
			((Control)label70).set_Location(new Point(2, 89));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(162, 39));
			((Control)label70).set_TabIndex(2);
			((Control)label70).set_Text("Entries in target after the first are \r\nconsistent with the entries in all\r\nthe comparisons");
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label68).set_Location(new Point(33, -2));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(103, 17));
			((Control)label68).set_TabIndex(1);
			((Control)label68).set_Text("Basic criteria");
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_ForeColor(Color.Teal);
			((Control)label67).set_Location(new Point(2, 13));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(164, 52));
			((Control)label67).set_TabIndex(0);
			((Control)label67).set_Text("First entry in Target at least about\r\n30% greater than:\r\n*  2nd  entry in target, and\r\n*  all entries in all Comparisons");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(648, 0));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(54, 26));
			((Control)label47).set_TabIndex(15);
			((Control)label47).set_Text("Running\r\nAverage");
			label47.set_TextAlign(ContentAlignment.TopCenter);
			((Control)panel7).set_BackColor(Color.PaleTurquoise);
			panel7.set_BorderStyle((BorderStyle)2);
			((Control)panel7).get_Controls().Add((Control)(object)opt9);
			((Control)panel7).get_Controls().Add((Control)(object)opt57);
			((Control)panel7).get_Controls().Add((Control)(object)opt41);
			((Control)panel7).get_Controls().Add((Control)(object)opt49);
			((Control)panel7).get_Controls().Add((Control)(object)opt33);
			((Control)panel7).get_Controls().Add((Control)(object)opt25);
			((Control)panel7).get_Controls().Add((Control)(object)opt17);
			((Control)panel7).set_Location(new Point(651, 28));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(48, 131));
			((Control)panel7).set_TabIndex(14);
			opt9.set_AutoCheck(false);
			((Control)opt9).set_AutoSize(true);
			((Control)opt9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt9).set_Location(new Point(3, 0));
			((Control)opt9).set_Name("opt9");
			((Control)opt9).set_Size(new Size(36, 17));
			((Control)opt9).set_TabIndex(0);
			((Control)opt9).set_Text(" 9");
			((ButtonBase)opt9).set_UseVisualStyleBackColor(true);
			((Control)opt9).add_Click((EventHandler)opt9_Click);
			opt57.set_AutoCheck(false);
			((Control)opt57).set_AutoSize(true);
			((Control)opt57).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt57).set_Location(new Point(3, 108));
			((Control)opt57).set_Name("opt57");
			((Control)opt57).set_Size(new Size(39, 17));
			((Control)opt57).set_TabIndex(6);
			((Control)opt57).set_Text("57");
			((ButtonBase)opt57).set_UseVisualStyleBackColor(true);
			((Control)opt57).add_Click((EventHandler)opt57_Click);
			opt41.set_AutoCheck(false);
			((Control)opt41).set_AutoSize(true);
			((Control)opt41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt41).set_Location(new Point(3, 72));
			((Control)opt41).set_Name("opt41");
			((Control)opt41).set_Size(new Size(39, 17));
			((Control)opt41).set_TabIndex(4);
			((Control)opt41).set_Text("41");
			((ButtonBase)opt41).set_UseVisualStyleBackColor(true);
			((Control)opt41).add_Click((EventHandler)opt41_Click);
			opt49.set_AutoCheck(false);
			((Control)opt49).set_AutoSize(true);
			((Control)opt49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt49).set_Location(new Point(3, 90));
			((Control)opt49).set_Name("opt49");
			((Control)opt49).set_Size(new Size(39, 17));
			((Control)opt49).set_TabIndex(5);
			((Control)opt49).set_Text("49");
			((ButtonBase)opt49).set_UseVisualStyleBackColor(true);
			((Control)opt49).add_Click((EventHandler)opt49_Click);
			opt33.set_AutoCheck(false);
			((Control)opt33).set_AutoSize(true);
			((Control)opt33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt33).set_Location(new Point(3, 54));
			((Control)opt33).set_Name("opt33");
			((Control)opt33).set_Size(new Size(39, 17));
			((Control)opt33).set_TabIndex(3);
			((Control)opt33).set_Text("33");
			((ButtonBase)opt33).set_UseVisualStyleBackColor(true);
			((Control)opt33).add_Click((EventHandler)opt33_Click);
			opt25.set_AutoCheck(false);
			((Control)opt25).set_AutoSize(true);
			opt25.set_Checked(true);
			((Control)opt25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt25).set_Location(new Point(3, 36));
			((Control)opt25).set_Name("opt25");
			((Control)opt25).set_Size(new Size(39, 17));
			((Control)opt25).set_TabIndex(2);
			opt25.set_TabStop(true);
			((Control)opt25).set_Text("25");
			((ButtonBase)opt25).set_UseVisualStyleBackColor(true);
			((Control)opt25).add_Click((EventHandler)opt25_Click);
			opt17.set_AutoCheck(false);
			((Control)opt17).set_AutoSize(true);
			((Control)opt17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt17).set_Location(new Point(3, 18));
			((Control)opt17).set_Name("opt17");
			((Control)opt17).set_Size(new Size(39, 17));
			((Control)opt17).set_TabIndex(1);
			((Control)opt17).set_Text("17");
			((ButtonBase)opt17).set_UseVisualStyleBackColor(true);
			((Control)opt17).add_Click((EventHandler)opt17_Click);
			((Control)lblNoCompStars).set_AutoSize(true);
			((Control)lblNoCompStars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblNoCompStars).set_ForeColor(Color.Red);
			((Control)lblNoCompStars).set_Location(new Point(254, 86));
			((Control)lblNoCompStars).set_Name("lblNoCompStars");
			((Control)lblNoCompStars).set_Size(new Size(393, 39));
			((Control)lblNoCompStars).set_TabIndex(13);
			((Control)lblNoCompStars).set_Text("The light curve data does not contain data for any comparison star.\r\nThis prevents a comparison with the noise of other stars.\r\nAs a result, any conclusion derived from the Target list is unreliable.");
			((Control)lblNoCompStars).set_Visible(false);
			((Control)lblTarget).set_AutoSize(true);
			((Control)lblTarget).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTarget).set_ForeColor(Color.FromArgb(0, 70, 0));
			((Control)lblTarget).set_Location(new Point(137, 12));
			((Control)lblTarget).set_Name("lblTarget");
			((Control)lblTarget).set_Size(new Size(125, 13));
			((Control)lblTarget).set_TabIndex(12);
			((Control)lblTarget).set_Text("Target  brightness = 1.00");
			picComp3.set_BorderStyle((BorderStyle)2);
			((Control)picComp3).set_Location(new Point(526, 26));
			((Control)picComp3).set_Name("picComp3");
			((Control)picComp3).set_Size(new Size(109, 50));
			picComp3.set_TabIndex(20);
			picComp3.set_TabStop(false);
			picComp2.set_BorderStyle((BorderStyle)2);
			((Control)picComp2).set_Location(new Point(398, 26));
			((Control)picComp2).set_Name("picComp2");
			((Control)picComp2).set_Size(new Size(109, 50));
			picComp2.set_TabIndex(19);
			picComp2.set_TabStop(false);
			picComp1.set_BorderStyle((BorderStyle)2);
			((Control)picComp1).set_Location(new Point(270, 26));
			((Control)picComp1).set_Name("picComp1");
			((Control)picComp1).set_Size(new Size(109, 50));
			picComp1.set_TabIndex(18);
			picComp1.set_TabStop(false);
			picTarget.set_BorderStyle((BorderStyle)2);
			((Control)picTarget).set_Location(new Point(145, 26));
			((Control)picTarget).set_Name("picTarget");
			((Control)picTarget).set_Size(new Size(109, 50));
			picTarget.set_TabIndex(17);
			picTarget.set_TabStop(false);
			((Control)lstComp3).set_BackColor(Color.LightCyan);
			((Control)lstComp3).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstComp3).set_FormattingEnabled(true);
			((Control)lstComp3).set_Location(new Point(526, 78));
			((Control)lstComp3).set_Name("lstComp3");
			((Control)lstComp3).set_Size(new Size(109, 82));
			((Control)lstComp3).set_TabIndex(3);
			toolTip.SetToolTip((Control)(object)lstComp3, "Right click to copy");
			((Control)lstComp3).add_MouseDown(new MouseEventHandler(lstComp3_MouseDown));
			((Control)lstComp2).set_BackColor(Color.LightCyan);
			((Control)lstComp2).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstComp2).set_FormattingEnabled(true);
			((Control)lstComp2).set_Location(new Point(398, 78));
			((Control)lstComp2).set_Name("lstComp2");
			((Control)lstComp2).set_Size(new Size(109, 82));
			((Control)lstComp2).set_TabIndex(2);
			toolTip.SetToolTip((Control)(object)lstComp2, "Right click to copy");
			((Control)lstComp2).add_MouseDown(new MouseEventHandler(lstComp2_MouseDown));
			((Control)lstComp1).set_BackColor(Color.LightCyan);
			((Control)lstComp1).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstComp1).set_FormattingEnabled(true);
			((Control)lstComp1).set_Location(new Point(270, 78));
			((Control)lstComp1).set_Name("lstComp1");
			((Control)lstComp1).set_Size(new Size(109, 82));
			((Control)lstComp1).set_TabIndex(1);
			toolTip.SetToolTip((Control)(object)lstComp1, "Right click to copy");
			((Control)lstComp1).add_MouseDown(new MouseEventHandler(lstComp1_MouseDown));
			((Control)lstTarget).set_BackColor(Color.Ivory);
			((Control)lstTarget).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstTarget).set_FormattingEnabled(true);
			((Control)lstTarget).set_Location(new Point(4, 78));
			((Control)lstTarget).set_Name("lstTarget");
			((Control)lstTarget).set_Size(new Size(250, 82));
			((Control)lstTarget).set_TabIndex(0);
			toolTip.SetToolTip((Control)(object)lstTarget, "Right click to copy");
			((Control)lstTarget).add_MouseDown(new MouseEventHandler(lstTarget_MouseDown));
			optComp1.set_AutoCheck(false);
			((Control)optComp1).set_AutoSize(true);
			optComp1.set_Checked(true);
			((Control)optComp1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optComp1).set_Location(new Point(343, 11));
			((Control)optComp1).set_Name("optComp1");
			((Control)optComp1).set_Size(new Size(43, 17));
			((Control)optComp1).set_TabIndex(73);
			optComp1.set_TabStop(true);
			((Control)optComp1).set_Text("Use");
			((ButtonBase)optComp1).set_UseVisualStyleBackColor(true);
			((Control)optComp1).add_Click((EventHandler)optComp1_Click);
			optComp2.set_AutoCheck(false);
			((Control)optComp2).set_AutoSize(true);
			optComp2.set_Checked(true);
			((Control)optComp2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optComp2).set_Location(new Point(472, 11));
			((Control)optComp2).set_Name("optComp2");
			((Control)optComp2).set_Size(new Size(43, 17));
			((Control)optComp2).set_TabIndex(74);
			optComp2.set_TabStop(true);
			((Control)optComp2).set_Text("Use");
			((ButtonBase)optComp2).set_TextAlign(ContentAlignment.BottomLeft);
			((ButtonBase)optComp2).set_UseVisualStyleBackColor(true);
			((Control)optComp2).add_Click((EventHandler)optComp2_Click);
			optComp3.set_AutoCheck(false);
			((Control)optComp3).set_AutoSize(true);
			optComp3.set_Checked(true);
			((Control)optComp3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optComp3).set_Location(new Point(598, 11));
			((Control)optComp3).set_Name("optComp3");
			((Control)optComp3).set_Size(new Size(43, 17));
			((Control)optComp3).set_TabIndex(75);
			optComp3.set_TabStop(true);
			((Control)optComp3).set_Text("Use");
			((ButtonBase)optComp3).set_TextAlign(ContentAlignment.TopLeft);
			((ButtonBase)optComp3).set_UseVisualStyleBackColor(true);
			((Control)optComp3).add_Click((EventHandler)optComp3_Click);
			((Control)lblComp1).set_AutoSize(true);
			((Control)lblComp1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblComp1).set_ForeColor(Color.Blue);
			((Control)lblComp1).set_Location(new Point(267, 14));
			((Control)lblComp1).set_Name("lblComp1");
			((Control)lblComp1).set_Size(new Size(76, 13));
			((Control)lblComp1).set_TabIndex(11);
			((Control)lblComp1).set_Text("Comp 1 = 1.00");
			((Control)lblComp2).set_AutoSize(true);
			((Control)lblComp2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblComp2).set_ForeColor(Color.Blue);
			((Control)lblComp2).set_Location(new Point(395, 14));
			((Control)lblComp2).set_Name("lblComp2");
			((Control)lblComp2).set_Size(new Size(76, 13));
			((Control)lblComp2).set_TabIndex(10);
			((Control)lblComp2).set_Text("Comp 2 = 1.00");
			((Control)lblComp3).set_AutoSize(true);
			((Control)lblComp3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblComp3).set_ForeColor(Color.Blue);
			((Control)lblComp3).set_Location(new Point(522, 14));
			((Control)lblComp3).set_Name("lblComp3");
			((Control)lblComp3).set_Size(new Size(76, 13));
			((Control)lblComp3).set_TabIndex(9);
			((Control)lblComp3).set_Text("Comp 3 = 1.00");
			((Control)lblHead2).set_AutoSize(true);
			((Control)lblHead2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHead2).set_Location(new Point(407, 0));
			((Control)lblHead2).set_Name("lblHead2");
			((Control)lblHead2).set_Size(new Size(91, 13));
			((Control)lblHead2).set_TabIndex(6);
			((Control)lblHead2).set_Text("Comparison #2");
			((Control)lblHead3).set_AutoSize(true);
			((Control)lblHead3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHead3).set_Location(new Point(535, 0));
			((Control)lblHead3).set_Name("lblHead3");
			((Control)lblHead3).set_Size(new Size(91, 13));
			((Control)lblHead3).set_TabIndex(5);
			((Control)lblHead3).set_Text("Comparison #3");
			((Control)lblHead1).set_AutoSize(true);
			((Control)lblHead1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHead1).set_Location(new Point(279, 0));
			((Control)lblHead1).set_Name("lblHead1");
			((Control)lblHead1).set_Size(new Size(91, 13));
			((Control)lblHead1).set_TabIndex(4);
			((Control)lblHead1).set_Text("Comparison #1");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label69).set_Location(new Point(38, 0));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(182, 13));
			((Control)label69).set_TabIndex(7);
			((Control)label69).set_Text("Target  (with comparison stars)");
			((Control)tabPage4).set_BackColor(Color.MintCream);
			((Control)tabPage4).get_Controls().Add((Control)(object)panelTab2);
			tabPage4.set_Location(new Point(4, 22));
			((Control)tabPage4).set_Name("tabPage4");
			((Control)tabPage4).set_Padding(new Padding(3));
			((Control)tabPage4).set_Size(new Size(892, 166));
			tabPage4.set_TabIndex(2);
			((Control)tabPage4).set_Text("3. Find events");
			tabPage4.set_UseVisualStyleBackColor(true);
			((Control)panelTab2).get_Controls().Add((Control)(object)panelFind);
			((Control)panelTab2).get_Controls().Add((Control)(object)grpMaxDurn);
			((Control)panelTab2).get_Controls().Add((Control)(object)grpExpected);
			((Control)panelTab2).get_Controls().Add((Control)(object)groupBox1);
			((Control)panelTab2).set_Location(new Point(1, 1));
			((Control)panelTab2).set_Name("panelTab2");
			((Control)panelTab2).set_Size(new Size(876, 162));
			((Control)panelTab2).set_TabIndex(50);
			((Control)panelFind).get_Controls().Add((Control)(object)cmdCancelFind);
			((Control)panelFind).get_Controls().Add((Control)(object)lblCurrentWidth);
			((Control)panelFind).get_Controls().Add((Control)(object)pBar);
			((Control)panelFind).get_Controls().Add((Control)(object)cmdFindPossible);
			((Control)panelFind).set_Location(new Point(579, 19));
			((Control)panelFind).set_Name("panelFind");
			((Control)panelFind).set_Size(new Size(177, 135));
			((Control)panelFind).set_TabIndex(53);
			((Control)cmdCancelFind).set_Location(new Point(58, 89));
			((Control)cmdCancelFind).set_Name("cmdCancelFind");
			((Control)cmdCancelFind).set_Size(new Size(57, 26));
			((Control)cmdCancelFind).set_TabIndex(52);
			((Control)cmdCancelFind).set_Text("Cancel");
			((ButtonBase)cmdCancelFind).set_UseVisualStyleBackColor(true);
			((Control)cmdCancelFind).set_Visible(false);
			((Control)cmdCancelFind).add_Click((EventHandler)cmdCancelFind_Click);
			((Control)cmdCancelFind).add_KeyDown(new KeyEventHandler(cmdCancelFind_KeyDown));
			((Control)cmdCancelFind).add_MouseClick(new MouseEventHandler(cmdCancelFind_MouseClick));
			((Control)cmdCancelFind).add_MouseDown(new MouseEventHandler(cmdCancelFind_MouseDown));
			((Control)lblCurrentWidth).set_AutoSize(true);
			((Control)lblCurrentWidth).set_Location(new Point(46, 50));
			((Control)lblCurrentWidth).set_Name("lblCurrentWidth");
			((Control)lblCurrentWidth).set_Size(new Size(13, 13));
			((Control)lblCurrentWidth).set_TabIndex(48);
			((Control)lblCurrentWidth).set_Text("0");
			((Control)lblCurrentWidth).set_Visible(false);
			((Control)pBar).set_Location(new Point(7, 64));
			((Control)pBar).set_Name("pBar");
			((Control)pBar).set_Size(new Size(153, 10));
			((Control)pBar).set_TabIndex(47);
			((Control)pBar).set_Visible(false);
			((Control)cmdFindPossible).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdFindPossible).set_Location(new Point(32, 5));
			((Control)cmdFindPossible).set_Name("cmdFindPossible");
			((Control)cmdFindPossible).set_Size(new Size(103, 39));
			((Control)cmdFindPossible).set_TabIndex(46);
			((Control)cmdFindPossible).set_Text("Find possible occultations");
			((ButtonBase)cmdFindPossible).set_UseVisualStyleBackColor(true);
			((Control)cmdFindPossible).add_Click((EventHandler)cmdFindPossible_Click);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)txtMaxFrames);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)cmbIntegration);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)label21);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)optPAL);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)optNTSC);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)label15);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)updnMaxDurn);
			((Control)grpMaxDurn).get_Controls().Add((Control)(object)label14);
			((Control)grpMaxDurn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpMaxDurn).set_Location(new Point(434, 5));
			((Control)grpMaxDurn).set_Name("grpMaxDurn");
			((Control)grpMaxDurn).set_Size(new Size(131, 150));
			((Control)grpMaxDurn).set_TabIndex(51);
			grpMaxDurn.set_TabStop(false);
			((Control)grpMaxDurn).set_Text("Maximum duration");
			((TextBoxBase)txtMaxFrames).set_BorderStyle((BorderStyle)1);
			((Control)txtMaxFrames).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMaxFrames).set_Location(new Point(74, 117));
			((Control)txtMaxFrames).set_Name("txtMaxFrames");
			((TextBoxBase)txtMaxFrames).set_ReadOnly(true);
			((Control)txtMaxFrames).set_Size(new Size(39, 20));
			((Control)txtMaxFrames).set_TabIndex(44);
			((Control)txtMaxFrames).set_Text("0");
			((Control)cmbIntegration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbIntegration).set_FormattingEnabled(true);
			cmbIntegration.get_Items().AddRange(new object[15]
			{
				"1", "2", "4", "6", "8", "12", "16", "24", "32", "48",
				"64", "96", "128", "256", "512"
			});
			((Control)cmbIntegration).set_Location(new Point(68, 83));
			((Control)cmbIntegration).set_Name("cmbIntegration");
			((Control)cmbIntegration).set_Size(new Size(51, 21));
			((Control)cmbIntegration).set_TabIndex(48);
			cmbIntegration.add_SelectedIndexChanged((EventHandler)cmbIntegration_SelectedIndexChanged);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(5, 80));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(62, 26));
			((Control)label21).set_TabIndex(49);
			((Control)label21).set_Text("Integration/\r\nbinning");
			((Control)optPAL).set_AutoSize(true);
			((Control)optPAL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPAL).set_Location(new Point(68, 53));
			((Control)optPAL).set_Name("optPAL");
			((Control)optPAL).set_Size(new Size(45, 17));
			((Control)optPAL).set_TabIndex(47);
			((Control)optPAL).set_Text("PAL");
			((ButtonBase)optPAL).set_UseVisualStyleBackColor(true);
			optPAL.add_CheckedChanged((EventHandler)optPAL_CheckedChanged);
			((Control)optNTSC).set_AutoSize(true);
			optNTSC.set_Checked(true);
			((Control)optNTSC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optNTSC).set_Location(new Point(8, 53));
			((Control)optNTSC).set_Name("optNTSC");
			((Control)optNTSC).set_Size(new Size(54, 17));
			((Control)optNTSC).set_TabIndex(46);
			optNTSC.set_TabStop(true);
			((Control)optNTSC).set_Text("NTSC");
			((ButtonBase)optNTSC).set_UseVisualStyleBackColor(true);
			optNTSC.add_CheckedChanged((EventHandler)optNTSC_CheckedChanged);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(7, 114));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(59, 26));
			((Control)label15).set_TabIndex(45);
			((Control)label15).set_Text("Max width \r\nto measure");
			((Control)updnMaxDurn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMaxDurn).set_Location(new Point(72, 23));
			updnMaxDurn.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnMaxDurn).set_Name("updnMaxDurn");
			((Control)updnMaxDurn).set_Size(new Size(40, 20));
			((Control)updnMaxDurn).set_TabIndex(43);
			updnMaxDurn.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnMaxDurn.add_ValueChanged((EventHandler)updnMaxDurn_ValueChanged);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(6, 20));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(53, 26));
			((Control)label14).set_TabIndex(42);
			((Control)label14).set_Text("Max Durn\r\n(secs)");
			label14.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)grpExpected).get_Controls().Add((Control)(object)txtMagDropLightLevel);
			((Control)grpExpected).get_Controls().Add((Control)(object)label20);
			((Control)grpExpected).get_Controls().Add((Control)(object)label12);
			((Control)grpExpected).get_Controls().Add((Control)(object)updnMagStar);
			((Control)grpExpected).get_Controls().Add((Control)(object)label11);
			((Control)grpExpected).get_Controls().Add((Control)(object)label13);
			((Control)grpExpected).get_Controls().Add((Control)(object)updnMagObject);
			((Control)grpExpected).get_Controls().Add((Control)(object)updnMagDrop);
			((Control)grpExpected).get_Controls().Add((Control)(object)label10);
			((Control)grpExpected).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpExpected).set_Location(new Point(268, 5));
			((Control)grpExpected).set_Name("grpExpected");
			((Control)grpExpected).set_Size(new Size(135, 150));
			((Control)grpExpected).set_TabIndex(50);
			grpExpected.set_TabStop(false);
			((Control)grpExpected).set_Text("Expected light drop");
			((TextBoxBase)txtMagDropLightLevel).set_BorderStyle((BorderStyle)1);
			((Control)txtMagDropLightLevel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMagDropLightLevel).set_Location(new Point(88, 120));
			((Control)txtMagDropLightLevel).set_Name("txtMagDropLightLevel");
			((TextBoxBase)txtMagDropLightLevel).set_ReadOnly(true);
			((Control)txtMagDropLightLevel).set_Size(new Size(39, 20));
			((Control)txtMagDropLightLevel).set_TabIndex(43);
			((Control)txtMagDropLightLevel).set_Text("0");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(5, 122));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(77, 13));
			((Control)label20).set_TabIndex(42);
			((Control)label20).set_Text("Expected level");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(17, 40));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(50, 13));
			((Control)label12).set_TabIndex(38);
			((Control)label12).set_Text("... of Star");
			updnMagStar.set_DecimalPlaces(1);
			((Control)updnMagStar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMagStar.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMagStar).set_Location(new Point(72, 36));
			updnMagStar.set_Maximum(new decimal(new int[4] { 19, 0, 0, 0 }));
			((Control)updnMagStar).set_Name("updnMagStar");
			((Control)updnMagStar).set_Size(new Size(51, 20));
			((Control)updnMagStar).set_TabIndex(35);
			updnMagStar.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnMagStar.add_ValueChanged((EventHandler)updnMagStar_ValueChanged);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(5, 65));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(62, 13));
			((Control)label11).set_TabIndex(37);
			((Control)label11).set_Text("... of Object");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(27, 90));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(40, 13));
			((Control)label13).set_TabIndex(39);
			((Control)label13).set_Text("... drop");
			updnMagObject.set_DecimalPlaces(1);
			((Control)updnMagObject).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMagObject.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMagObject).set_Location(new Point(72, 61));
			updnMagObject.set_Maximum(new decimal(new int[4] { 19, 0, 0, 0 }));
			((Control)updnMagObject).set_Name("updnMagObject");
			((Control)updnMagObject).set_Size(new Size(51, 20));
			((Control)updnMagObject).set_TabIndex(40);
			updnMagObject.add_ValueChanged((EventHandler)updnMagObject_ValueChanged);
			updnMagDrop.set_DecimalPlaces(2);
			((Control)updnMagDrop).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMagDrop.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMagDrop).set_Location(new Point(71, 86));
			updnMagDrop.set_Maximum(new decimal(new int[4] { 19, 0, 0, 0 }));
			((Control)updnMagDrop).set_Name("updnMagDrop");
			((Control)updnMagDrop).set_Size(new Size(51, 20));
			((Control)updnMagDrop).set_TabIndex(41);
			updnMagDrop.add_ValueChanged((EventHandler)updnMagDrop_ValueChanged);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(7, 16));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(62, 13));
			((Control)label10).set_TabIndex(36);
			((Control)label10).set_Text("Magnitudes");
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdSetLevelsAutomatic);
			((Control)groupBox1).get_Controls().Add((Control)(object)label22);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtExpectedSN);
			((Control)groupBox1).get_Controls().Add((Control)(object)optPossOccLevel);
			((Control)groupBox1).get_Controls().Add((Control)(object)optFullLight);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtFullPosn);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtFullLightLevel);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtOccLevelPos);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtOccultedLightLevel);
			((Control)groupBox1).get_Controls().Add((Control)(object)label8);
			((Control)groupBox1).get_Controls().Add((Control)(object)label9);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(5, 5));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(250, 150));
			((Control)groupBox1).set_TabIndex(49);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Set Levels");
			((Control)cmdSetLevelsAutomatic).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetLevelsAutomatic).set_Location(new Point(177, 24));
			((Control)cmdSetLevelsAutomatic).set_Name("cmdSetLevelsAutomatic");
			((Control)cmdSetLevelsAutomatic).set_Size(new Size(65, 34));
			((Control)cmdSetLevelsAutomatic).set_TabIndex(38);
			((Control)cmdSetLevelsAutomatic).set_Text("automatic\r\nset levels");
			((ButtonBase)cmdSetLevelsAutomatic).set_UseVisualStyleBackColor(true);
			((Control)cmdSetLevelsAutomatic).add_Click((EventHandler)cmdSetLevelsAutomatic_Click);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(195, 66));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(27, 13));
			((Control)label22).set_TabIndex(37);
			((Control)label22).set_Text("S/N");
			((TextBoxBase)txtExpectedSN).set_BorderStyle((BorderStyle)1);
			((Control)txtExpectedSN).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtExpectedSN).set_Location(new Point(190, 83));
			((Control)txtExpectedSN).set_Name("txtExpectedSN");
			((TextBoxBase)txtExpectedSN).set_ReadOnly(true);
			((Control)txtExpectedSN).set_Size(new Size(39, 20));
			((Control)txtExpectedSN).set_TabIndex(36);
			((Control)txtExpectedSN).set_Text("0");
			((Control)optPossOccLevel).set_AutoSize(true);
			optPossOccLevel.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optPossOccLevel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPossOccLevel).set_Location(new Point(6, 58));
			((Control)optPossOccLevel).set_Name("optPossOccLevel");
			((Control)optPossOccLevel).set_Size(new Size(64, 30));
			((Control)optPossOccLevel).set_TabIndex(26);
			((Control)optPossOccLevel).set_Text("Possible\r\nOccn");
			((ButtonBase)optPossOccLevel).set_UseVisualStyleBackColor(true);
			((Control)optFullLight).set_AutoSize(true);
			optFullLight.set_CheckAlign(ContentAlignment.MiddleRight);
			optFullLight.set_Checked(true);
			((Control)optFullLight).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optFullLight).set_Location(new Point(6, 34));
			((Control)optFullLight).set_Name("optFullLight");
			((Control)optFullLight).set_Size(new Size(63, 17));
			((Control)optFullLight).set_TabIndex(25);
			optFullLight.set_TabStop(true);
			((Control)optFullLight).set_Text("Full light");
			((ButtonBase)optFullLight).set_UseVisualStyleBackColor(true);
			((TextBoxBase)txtFullPosn).set_BorderStyle((BorderStyle)1);
			((Control)txtFullPosn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFullPosn).set_Location(new Point(77, 32));
			((Control)txtFullPosn).set_Name("txtFullPosn");
			((TextBoxBase)txtFullPosn).set_ReadOnly(true);
			((Control)txtFullPosn).set_Size(new Size(39, 20));
			((Control)txtFullPosn).set_TabIndex(29);
			((Control)txtFullPosn).set_Text("0");
			((TextBoxBase)txtFullLightLevel).set_BorderStyle((BorderStyle)1);
			((Control)txtFullLightLevel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFullLightLevel).set_Location(new Point(127, 32));
			((Control)txtFullLightLevel).set_Name("txtFullLightLevel");
			((TextBoxBase)txtFullLightLevel).set_ReadOnly(true);
			((Control)txtFullLightLevel).set_Size(new Size(39, 20));
			((Control)txtFullLightLevel).set_TabIndex(30);
			((Control)txtFullLightLevel).set_Text("0");
			((TextBoxBase)txtOccLevelPos).set_BorderStyle((BorderStyle)1);
			((Control)txtOccLevelPos).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtOccLevelPos).set_Location(new Point(77, 63));
			((Control)txtOccLevelPos).set_Name("txtOccLevelPos");
			((TextBoxBase)txtOccLevelPos).set_ReadOnly(true);
			((Control)txtOccLevelPos).set_Size(new Size(39, 20));
			((Control)txtOccLevelPos).set_TabIndex(28);
			((Control)txtOccLevelPos).set_Text("0");
			((TextBoxBase)txtOccultedLightLevel).set_BorderStyle((BorderStyle)1);
			((Control)txtOccultedLightLevel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtOccultedLightLevel).set_Location(new Point(127, 63));
			((Control)txtOccultedLightLevel).set_Name("txtOccultedLightLevel");
			((TextBoxBase)txtOccultedLightLevel).set_ReadOnly(true);
			((Control)txtOccultedLightLevel).set_Size(new Size(39, 20));
			((Control)txtOccultedLightLevel).set_TabIndex(31);
			((Control)txtOccultedLightLevel).set_Text("0");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(77, 15));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(44, 13));
			((Control)label8).set_TabIndex(33);
			((Control)label8).set_Text("Position");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(127, 15));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(33, 13));
			((Control)label9).set_TabIndex(34);
			((Control)label9).set_Text("Level");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(1, 114));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(248, 26));
			((Control)label2).set_TabIndex(35);
			((Control)label2).set_Text("To set a level, click Mouse at a point along the plot\r\n just before or after, and during, the occultation.");
			((Control)tabPage5).get_Controls().Add((Control)(object)panelTab3);
			tabPage5.set_Location(new Point(4, 22));
			((Control)tabPage5).set_Name("tabPage5");
			((Control)tabPage5).set_Size(new Size(892, 166));
			tabPage5.set_TabIndex(3);
			((Control)tabPage5).set_Text("4. Select event to analyse");
			tabPage5.set_UseVisualStyleBackColor(true);
			((Control)panelTab3).set_BackColor(Color.LightCyan);
			((Control)panelTab3).get_Controls().Add((Control)(object)panelEvents);
			((Control)panelTab3).set_Location(new Point(1, 1));
			((Control)panelTab3).set_Name("panelTab3");
			((Control)panelTab3).set_Size(new Size(881, 162));
			((Control)panelTab3).set_TabIndex(51);
			((Control)panelEvents).get_Controls().Add((Control)(object)panelExpectedMagDrop);
			((Control)panelEvents).get_Controls().Add((Control)(object)label19);
			((Control)panelEvents).get_Controls().Add((Control)(object)cmdSetAllToMiss);
			((Control)panelEvents).get_Controls().Add((Control)(object)label46);
			((Control)panelEvents).get_Controls().Add((Control)(object)label18);
			((Control)panelEvents).get_Controls().Add((Control)(object)label17);
			((Control)panelEvents).get_Controls().Add((Control)(object)label16);
			((Control)panelEvents).set_Location(new Point(13, 4));
			((Control)panelEvents).set_Name("panelEvents");
			((Control)panelEvents).set_Size(new Size(858, 153));
			((Control)panelEvents).set_TabIndex(50);
			((Control)panelExpectedMagDrop).get_Controls().Add((Control)(object)label66);
			((Control)panelExpectedMagDrop).get_Controls().Add((Control)(object)updnExpectedMagDrop);
			((Control)panelExpectedMagDrop).get_Controls().Add((Control)(object)label65);
			((Control)panelExpectedMagDrop).set_Location(new Point(674, 8));
			((Control)panelExpectedMagDrop).set_Name("panelExpectedMagDrop");
			((Control)panelExpectedMagDrop).set_Size(new Size(112, 130));
			((Control)panelExpectedMagDrop).set_TabIndex(9);
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Location(new Point(2, 84));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(104, 39));
			((Control)label66).set_TabIndex(8);
			((Control)label66).set_Text("Used to generate an\r\n'expected' SN value\r\nfor Non-Events");
			updnExpectedMagDrop.set_DecimalPlaces(2);
			updnExpectedMagDrop.set_Increment(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnExpectedMagDrop).set_Location(new Point(32, 44));
			updnExpectedMagDrop.set_Maximum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnExpectedMagDrop).set_Name("updnExpectedMagDrop");
			((Control)updnExpectedMagDrop).set_Size(new Size(44, 20));
			((Control)updnExpectedMagDrop).set_TabIndex(6);
			updnExpectedMagDrop.set_Value(new decimal(new int[4] { 20, 0, 0, 131072 }));
			updnExpectedMagDrop.add_ValueChanged((EventHandler)updnExpectedMagDrop_ValueChanged);
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label65).set_Location(new Point(5, 9));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(94, 26));
			((Control)label65).set_TabIndex(7);
			((Control)label65).set_Text("Expected \r\nmagnitude drop");
			label65.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(159, 1));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(35, 13));
			((Control)label19).set_TabIndex(3);
			((Control)label19).set_Text("Width");
			((Control)cmdSetAllToMiss).set_Location(new Point(525, 46));
			((Control)cmdSetAllToMiss).set_Name("cmdSetAllToMiss");
			((Control)cmdSetAllToMiss).set_Size(new Size(77, 41));
			((Control)cmdSetAllToMiss).set_TabIndex(5);
			((Control)cmdSetAllToMiss).set_Text("Set ALL as\r\n'Non-Event'");
			((ButtonBase)cmdSetAllToMiss).set_UseVisualStyleBackColor(true);
			((Control)cmdSetAllToMiss).add_Click((EventHandler)cmdSetAllToMiss_Click);
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Location(new Point(394, 1));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(95, 13));
			((Control)label46).set_TabIndex(4);
			((Control)label46).set_Text("Set as 'Non-Event'");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(102, 1));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(31, 13));
			((Control)label18).set_TabIndex(2);
			((Control)label18).set_Text("Posn");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(34, 1));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(49, 13));
			((Control)label17).set_TabIndex(1);
			((Control)label17).set_Text("Add/Edit");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(1, 1));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(25, 13));
			((Control)label16).set_TabIndex(0);
			((Control)label16).set_Text("Plot");
			((Control)tabPage6).set_BackColor(Color.Linen);
			((Control)tabPage6).get_Controls().Add((Control)(object)panelTab4);
			((Control)tabPage6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			tabPage6.set_Location(new Point(4, 22));
			((Control)tabPage6).set_Name("tabPage6");
			((Control)tabPage6).set_Padding(new Padding(3));
			((Control)tabPage6).set_Size(new Size(892, 166));
			tabPage6.set_TabIndex(1);
			((Control)tabPage6).set_Text("5. Analyse");
			tabPage6.set_UseVisualStyleBackColor(true);
			((Control)panelTab4).get_Controls().Add((Control)(object)panelDR);
			((Control)panelTab4).get_Controls().Add((Control)(object)panelUncertainty);
			((Control)panelTab4).get_Controls().Add((Control)(object)panelTransitions);
			((Control)panelTab4).get_Controls().Add((Control)(object)panelGetLocations);
			((Control)panelTab4).get_Controls().Add((Control)(object)panelChi2);
			((Control)panelTab4).get_Controls().Add((Control)(object)panelMonteCarlo);
			((Control)panelTab4).get_Controls().Add((Control)(object)panelAdjustRegions);
			((Control)panelTab4).set_Location(new Point(0, 1));
			((Control)panelTab4).set_Name("panelTab4");
			((Control)panelTab4).set_Size(new Size(890, 162));
			((Control)panelTab4).set_TabIndex(20);
			((Control)panelDR).get_Controls().Add((Control)(object)label25);
			((Control)panelDR).get_Controls().Add((Control)(object)label24);
			((Control)panelDR).set_Location(new Point(703, 37));
			((Control)panelDR).set_Name("panelDR");
			((Control)panelDR).set_Size(new Size(26, 115));
			((Control)panelDR).set_TabIndex(60);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(0, 74));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(24, 24));
			((Control)label25).set_TabIndex(26);
			((Control)label25).set_Text("R");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(0, 12));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(24, 24));
			((Control)label24).set_TabIndex(25);
			((Control)label24).set_Text("D");
			((Control)panelUncertainty).get_Controls().Add((Control)(object)label27);
			((Control)panelUncertainty).get_Controls().Add((Control)(object)picRUncert);
			((Control)panelUncertainty).get_Controls().Add((Control)(object)picDUncert);
			((Control)panelUncertainty).set_Location(new Point(723, 0));
			((Control)panelUncertainty).set_Name("panelUncertainty");
			((Control)panelUncertainty).set_Size(new Size(166, 159));
			((Control)panelUncertainty).set_TabIndex(59);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(30, 11));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(108, 13));
			((Control)label27).set_TabIndex(36);
			((Control)label27).set_Text("Uncertainty range");
			((Control)picRUncert).set_BackColor(Color.White);
			picRUncert.set_BorderStyle((BorderStyle)1);
			((Control)picRUncert).set_Location(new Point(6, 96));
			((Control)picRUncert).set_Name("picRUncert");
			((Control)picRUncert).set_Size(new Size(156, 60));
			picRUncert.set_TabIndex(35);
			picRUncert.set_TabStop(false);
			((Control)picDUncert).set_BackColor(Color.White);
			picDUncert.set_BorderStyle((BorderStyle)1);
			((Control)picDUncert).set_Location(new Point(6, 31));
			((Control)picDUncert).set_Name("picDUncert");
			((Control)picDUncert).set_Size(new Size(156, 60));
			picDUncert.set_TabIndex(34);
			picDUncert.set_TabStop(false);
			((Control)panelTransitions).get_Controls().Add((Control)(object)label38);
			((Control)panelTransitions).get_Controls().Add((Control)(object)label23);
			((Control)panelTransitions).get_Controls().Add((Control)(object)label28);
			((Control)panelTransitions).get_Controls().Add((Control)(object)picChiR);
			((Control)panelTransitions).get_Controls().Add((Control)(object)picChiD);
			((Control)panelTransitions).get_Controls().Add((Control)(object)label37);
			((Control)panelTransitions).set_Location(new Point(499, 0));
			((Control)panelTransitions).set_Name("panelTransitions");
			((Control)panelTransitions).set_Size(new Size(204, 161));
			((Control)panelTransitions).set_TabIndex(58);
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(32, 2));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(10, 9));
			((Control)label38).set_TabIndex(57);
			((Control)label38).set_Text("2");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(39, 5));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(147, 13));
			((Control)label23).set_TabIndex(24);
			((Control)label23).set_Text("-  for different transitions");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Symbol", 12f, FontStyle.Bold, GraphicsUnit.Point, 2));
			((Control)label28).set_Location(new Point(18, -1));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(19, 20));
			((Control)label28).set_TabIndex(37);
			((Control)label28).set_Text("c");
			((Control)picChiR).set_BackColor(Color.White);
			picChiR.set_BorderStyle((BorderStyle)1);
			((Control)picChiR).set_Location(new Point(4, 96));
			((Control)picChiR).set_Name("picChiR");
			((Control)picChiR).set_Size(new Size(197, 60));
			picChiR.set_TabIndex(23);
			picChiR.set_TabStop(false);
			((Control)picChiR).add_MouseClick(new MouseEventHandler(picChiR_MouseClick));
			((Control)picChiD).set_BackColor(Color.White);
			picChiD.set_BorderStyle((BorderStyle)1);
			((Control)picChiD).set_Location(new Point(4, 31));
			((Control)picChiD).set_Name("picChiD");
			((Control)picChiD).set_Size(new Size(197, 60));
			picChiD.set_TabIndex(22);
			picChiD.set_TabStop(false);
			((Control)picChiD).add_MouseClick(new MouseEventHandler(picChiD_MouseClick));
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(30, 18));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(145, 13));
			((Control)label37).set_TabIndex(56);
			((Control)label37).set_Text("Click histogram for uncertainty");
			((Control)panelGetLocations).set_BackColor(Color.Honeydew);
			panelGetLocations.set_BorderStyle((BorderStyle)1);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)label64);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)label63);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)updnConfidenceR);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)panel6);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)label61);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)updnConfidenceD);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)label30);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)lblDframe);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)panelSN);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)lblRframe);
			((Control)panelGetLocations).get_Controls().Add((Control)(object)cmdGetDandR);
			((Control)panelGetLocations).set_Location(new Point(307, 0));
			((Control)panelGetLocations).set_Name("panelGetLocations");
			((Control)panelGetLocations).set_Size(new Size(191, 160));
			((Control)panelGetLocations).set_TabIndex(55);
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Location(new Point(138, 20));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(15, 13));
			((Control)label64).set_TabIndex(51);
			((Control)label64).set_Text("R");
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Location(new Point(89, 20));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(15, 13));
			((Control)label63).set_TabIndex(50);
			((Control)label63).set_Text("D");
			updnConfidenceR.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnConfidenceR).set_Location(new Point(153, 16));
			updnConfidenceR.set_Maximum(new decimal(new int[4] { 95, 0, 0, 0 }));
			updnConfidenceR.set_Minimum(new decimal(new int[4] { 50, 0, 0, 0 }));
			((Control)updnConfidenceR).set_Name("updnConfidenceR");
			((Control)updnConfidenceR).set_Size(new Size(32, 20));
			((Control)updnConfidenceR).set_TabIndex(49);
			updnConfidenceR.set_Value(new decimal(new int[4] { 95, 0, 0, 0 }));
			updnConfidenceR.add_ValueChanged((EventHandler)updnConfidenceR_ValueChanged);
			((Control)panel6).set_BackColor(Color.Thistle);
			((Control)panel6).get_Controls().Add((Control)(object)label62);
			((Control)panel6).get_Controls().Add((Control)(object)chkShowMeasurementMeans);
			((Control)panel6).get_Controls().Add((Control)(object)chkShowCrossCorr);
			((Control)panel6).get_Controls().Add((Control)(object)chkShowErrorBars);
			((Control)panel6).set_Location(new Point(0, 91));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(189, 33));
			((Control)panel6).set_TabIndex(48);
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(44, 3));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(37, 13));
			((Control)label62).set_TabIndex(47);
			((Control)label62).set_Text("Show:");
			((Control)chkShowMeasurementMeans).set_AutoSize(true);
			chkShowMeasurementMeans.set_Checked(Settings.Default.AOTA_ShowMeasurementMeans);
			((Control)chkShowMeasurementMeans).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AOTA_ShowMeasurementMeans", true, (DataSourceUpdateMode)1));
			((Control)chkShowMeasurementMeans).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShowMeasurementMeans).set_Location(new Point(10, 17));
			((Control)chkShowMeasurementMeans).set_Name("chkShowMeasurementMeans");
			((Control)chkShowMeasurementMeans).set_Size(new Size(91, 17));
			((Control)chkShowMeasurementMeans).set_TabIndex(43);
			((Control)chkShowMeasurementMeans).set_Text("meas. means ");
			((ButtonBase)chkShowMeasurementMeans).set_UseVisualStyleBackColor(true);
			chkShowMeasurementMeans.add_CheckedChanged((EventHandler)chkShowMeasurementMeans_CheckedChanged);
			((Control)chkShowCrossCorr).set_AutoSize(true);
			chkShowCrossCorr.set_Checked(Settings.Default.AOTA_ShowCrossCorrln);
			((Control)chkShowCrossCorr).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AOTA_ShowCrossCorrln", true, (DataSourceUpdateMode)1));
			((Control)chkShowCrossCorr).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShowCrossCorr).set_Location(new Point(106, 2));
			((Control)chkShowCrossCorr).set_Name("chkShowCrossCorr");
			((Control)chkShowCrossCorr).set_Size(new Size(78, 17));
			((Control)chkShowCrossCorr).set_TabIndex(42);
			((Control)chkShowCrossCorr).set_Text("cross-corrln");
			((ButtonBase)chkShowCrossCorr).set_UseVisualStyleBackColor(true);
			chkShowCrossCorr.add_CheckedChanged((EventHandler)chkShowCrossCorr_CheckedChanged);
			((Control)chkShowErrorBars).set_AutoSize(true);
			chkShowErrorBars.set_Checked(Settings.Default.AOTA_ShowErrorBars);
			chkShowErrorBars.set_CheckState((CheckState)1);
			((Control)chkShowErrorBars).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AOTA_ShowErrorBars", true, (DataSourceUpdateMode)1));
			((Control)chkShowErrorBars).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShowErrorBars).set_Location(new Point(106, 17));
			((Control)chkShowErrorBars).set_Name("chkShowErrorBars");
			((Control)chkShowErrorBars).set_Size(new Size(70, 17));
			((Control)chkShowErrorBars).set_TabIndex(44);
			((Control)chkShowErrorBars).set_Text("error bars");
			((ButtonBase)chkShowErrorBars).set_UseVisualStyleBackColor(true);
			chkShowErrorBars.add_CheckedChanged((EventHandler)chkShowErrorBars_CheckedChanged);
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(98, 0));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(84, 13));
			((Control)label61).set_TabIndex(46);
			((Control)label61).set_Text("Confidence %");
			updnConfidenceD.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnConfidenceD).set_Location(new Point(104, 16));
			updnConfidenceD.set_Maximum(new decimal(new int[4] { 95, 0, 0, 0 }));
			updnConfidenceD.set_Minimum(new decimal(new int[4] { 50, 0, 0, 0 }));
			((Control)updnConfidenceD).set_Name("updnConfidenceD");
			((Control)updnConfidenceD).set_Size(new Size(32, 20));
			((Control)updnConfidenceD).set_TabIndex(45);
			updnConfidenceD.set_Value(new decimal(new int[4] { 95, 0, 0, 0 }));
			updnConfidenceD.add_ValueChanged((EventHandler)updnConfidence_ValueChanged);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(9, 41));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(173, 17));
			((Control)label30).set_TabIndex(41);
			((Control)label30).set_Text("Location of occultation");
			((Control)lblDframe).set_AutoSize(true);
			((Control)lblDframe).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDframe).set_Location(new Point(9, 61));
			((Control)lblDframe).set_Name("lblDframe");
			((Control)lblDframe).set_Size(new Size(16, 13));
			((Control)lblDframe).set_TabIndex(32);
			((Control)lblDframe).set_Text("D");
			((Control)panelSN).set_BackColor(Color.PowderBlue);
			((Control)panelSN).get_Controls().Add((Control)(object)label40);
			((Control)panelSN).get_Controls().Add((Control)(object)label3);
			((Control)panelSN).get_Controls().Add((Control)(object)label4);
			((Control)panelSN).get_Controls().Add((Control)(object)txtSN_at_R);
			((Control)panelSN).get_Controls().Add((Control)(object)txtSN_at_D);
			((Control)panelSN).set_Location(new Point(0, 125));
			((Control)panelSN).set_Name("panelSN");
			((Control)panelSN).set_Size(new Size(189, 24));
			((Control)panelSN).set_TabIndex(38);
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(109, 4));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(30, 13));
			((Control)label40).set_TabIndex(29);
			((Control)label40).set_Text("at R");
			((Control)lblRframe).set_AutoSize(true);
			((Control)lblRframe).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblRframe).set_Location(new Point(9, 75));
			((Control)lblRframe).set_Name("lblRframe");
			((Control)lblRframe).set_Size(new Size(16, 13));
			((Control)lblRframe).set_TabIndex(33);
			((Control)lblRframe).set_Text("R");
			((Control)cmdGetDandR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGetDandR).set_Location(new Point(7, 4));
			((Control)cmdGetDandR).set_Name("cmdGetDandR");
			((Control)cmdGetDandR).set_Size(new Size(80, 36));
			((Control)cmdGetDandR).set_TabIndex(30);
			((Control)cmdGetDandR).set_Text("Get event locations");
			((ButtonBase)cmdGetDandR).set_UseVisualStyleBackColor(true);
			((Control)cmdGetDandR).add_Click((EventHandler)cmdGetDandR_Click);
			((Control)panelChi2).set_BackColor(Color.PapayaWhip);
			panelChi2.set_BorderStyle((BorderStyle)1);
			((Control)panelChi2).get_Controls().Add((Control)(object)updnTransitionNumber);
			((Control)panelChi2).get_Controls().Add((Control)(object)panel3);
			((Control)panelChi2).get_Controls().Add((Control)(object)label1);
			((Control)panelChi2).get_Controls().Add((Control)(object)label35);
			((Control)panelChi2).get_Controls().Add((Control)(object)label36);
			((Control)panelChi2).get_Controls().Add((Control)(object)label32);
			((Control)panelChi2).set_Location(new Point(148, 3));
			((Control)panelChi2).set_Name("panelChi2");
			((Control)panelChi2).set_Size(new Size(158, 66));
			((Control)panelChi2).set_TabIndex(54);
			((Control)updnTransitionNumber).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "NumberOfTransitions", true, (DataSourceUpdateMode)1));
			updnTransitionNumber.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnTransitionNumber).set_Location(new Point(18, 39));
			updnTransitionNumber.set_Maximum(new decimal(new int[4] { 40, 0, 0, 0 }));
			updnTransitionNumber.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnTransitionNumber).set_Name("updnTransitionNumber");
			((Control)updnTransitionNumber).set_Size(new Size(43, 20));
			((Control)updnTransitionNumber).set_TabIndex(45);
			updnTransitionNumber.set_Value(Settings.Default.NumberOfTransitions);
			updnTransitionNumber.add_ValueChanged((EventHandler)updnTransitionNumber_ValueChanged);
			((Control)panel3).set_BackColor(Color.NavajoWhite);
			((Control)panel3).get_Controls().Add((Control)(object)label54);
			((Control)panel3).get_Controls().Add((Control)(object)optChi2StdDevn);
			((Control)panel3).get_Controls().Add((Control)(object)optChi2Variance);
			((Control)panel3).set_Location(new Point(77, 18));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(79, 47));
			((Control)panel3).set_TabIndex(57);
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(8, 2));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(70, 13));
			((Control)label54).set_TabIndex(56);
			((Control)label54).set_Text("Vary noise by");
			((Control)optChi2StdDevn).set_AutoSize(true);
			((Control)optChi2StdDevn).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optChi2StdDevn).set_Location(new Point(8, 29));
			((Control)optChi2StdDevn).set_Name("optChi2StdDevn");
			((Control)optChi2StdDevn).set_Size(new Size(70, 17));
			((Control)optChi2StdDevn).set_TabIndex(55);
			((Control)optChi2StdDevn).set_Text("Std Devn");
			((ButtonBase)optChi2StdDevn).set_UseVisualStyleBackColor(true);
			((Control)optChi2Variance).set_AutoSize(true);
			optChi2Variance.set_Checked(true);
			((Control)optChi2Variance).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optChi2Variance).set_Location(new Point(8, 13));
			((Control)optChi2Variance).set_Name("optChi2Variance");
			((Control)optChi2Variance).set_Size(new Size(66, 17));
			((Control)optChi2Variance).set_TabIndex(54);
			optChi2Variance.set_TabStop(true);
			((Control)optChi2Variance).set_Text("Variance");
			((ButtonBase)optChi2Variance).set_UseVisualStyleBackColor(true);
			optChi2Variance.add_CheckedChanged((EventHandler)optChi2Variance_CheckedChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(20, 3));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(10, 9));
			((Control)label1).set_TabIndex(53);
			((Control)label1).set_Text("2");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Symbol", 12f, FontStyle.Bold, GraphicsUnit.Point, 2));
			((Control)label35).set_Location(new Point(6, 0));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(19, 20));
			((Control)label35).set_TabIndex(51);
			((Control)label35).set_Text("c");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(28, 6));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(52, 13));
			((Control)label36).set_TabIndex(52);
			((Control)label36).set_Text("analysis");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(7, 25));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(64, 13));
			((Control)label32).set_TabIndex(46);
			((Control)label32).set_Text("# transitions");
			((Control)panelMonteCarlo).set_BackColor(Color.Cornsilk);
			panelMonteCarlo.set_BorderStyle((BorderStyle)1);
			((Control)panelMonteCarlo).get_Controls().Add((Control)(object)updnMonteCarlo);
			((Control)panelMonteCarlo).get_Controls().Add((Control)(object)panel4);
			((Control)panelMonteCarlo).get_Controls().Add((Control)(object)label49);
			((Control)panelMonteCarlo).get_Controls().Add((Control)(object)label34);
			((Control)panelMonteCarlo).get_Controls().Add((Control)(object)label33);
			((Control)panelMonteCarlo).get_Controls().Add((Control)(object)updnSDLimit);
			((Control)panelMonteCarlo).get_Controls().Add((Control)(object)label31);
			((Control)panelMonteCarlo).set_Location(new Point(148, 73));
			((Control)panelMonteCarlo).set_Name("panelMonteCarlo");
			((Control)panelMonteCarlo).set_Size(new Size(158, 87));
			((Control)panelMonteCarlo).set_TabIndex(53);
			((Control)updnMonteCarlo).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MonteCarloTrialNos", true, (DataSourceUpdateMode)1));
			updnMonteCarlo.set_Increment(new decimal(new int[4] { 100, 0, 0, 0 }));
			((Control)updnMonteCarlo).set_Location(new Point(6, 16));
			updnMonteCarlo.set_Maximum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnMonteCarlo.set_Minimum(new decimal(new int[4] { 100, 0, 0, 0 }));
			((Control)updnMonteCarlo).set_Name("updnMonteCarlo");
			((Control)updnMonteCarlo).set_Size(new Size(47, 20));
			((Control)updnMonteCarlo).set_TabIndex(43);
			toolTip.SetToolTip((Control)(object)updnMonteCarlo, "The larger the number or trials, the better the estimation of the uncertainties - but at a cost of slower response time.");
			updnMonteCarlo.set_Value(Settings.Default.MonteCarloTrialNos);
			updnMonteCarlo.add_ValueChanged((EventHandler)updnMonteCarlo_ValueChanged);
			((Control)panel4).set_BackColor(Color.NavajoWhite);
			((Control)panel4).get_Controls().Add((Control)(object)optMonteCarloMeasuredSignal);
			((Control)panel4).get_Controls().Add((Control)(object)optMonteCarloTestSignal);
			((Control)panel4).set_Location(new Point(77, 34));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(81, 31));
			((Control)panel4).set_TabIndex(54);
			((Control)optMonteCarloMeasuredSignal).set_AutoSize(true);
			((Control)optMonteCarloMeasuredSignal).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optMonteCarloMeasuredSignal).set_Location(new Point(5, 14));
			((Control)optMonteCarloMeasuredSignal).set_Name("optMonteCarloMeasuredSignal");
			((Control)optMonteCarloMeasuredSignal).set_Size(new Size(71, 17));
			((Control)optMonteCarloMeasuredSignal).set_TabIndex(52);
			((Control)optMonteCarloMeasuredSignal).set_Text("measured");
			((ButtonBase)optMonteCarloMeasuredSignal).set_UseVisualStyleBackColor(true);
			((Control)optMonteCarloTestSignal).set_AutoSize(true);
			optMonteCarloTestSignal.set_Checked(true);
			((Control)optMonteCarloTestSignal).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optMonteCarloTestSignal).set_Location(new Point(5, -1));
			((Control)optMonteCarloTestSignal).set_Name("optMonteCarloTestSignal");
			((Control)optMonteCarloTestSignal).set_Size(new Size(72, 17));
			((Control)optMonteCarloTestSignal).set_TabIndex(51);
			optMonteCarloTestSignal.set_TabStop(true);
			((Control)optMonteCarloTestSignal).set_Text("test signal");
			((ButtonBase)optMonteCarloTestSignal).set_TextImageRelation((TextImageRelation)2);
			((ButtonBase)optMonteCarloTestSignal).set_UseVisualStyleBackColor(true);
			optMonteCarloTestSignal.add_CheckedChanged((EventHandler)optMonteCarloTestSignal_CheckedChanged);
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_BackColor(Color.NavajoWhite);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(-3, 42));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(82, 13));
			((Control)label49).set_TabIndex(53);
			((Control)label49).set_Text("Noise applied to");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(3, 2));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(141, 13));
			((Control)label34).set_TabIndex(50);
			((Control)label34).set_Text("Monte Carlo parameters");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(49, 68));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(109, 13));
			((Control)label33).set_TabIndex(48);
			((Control)label33).set_Text("Std Dev limit on noise");
			toolTip.SetToolTip((Control)(object)label33, "When running the Monte Carlo trials, the artificial noise is limited to being less than this value");
			((Control)updnSDLimit).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MonteCarloNoiseLimit", true, (DataSourceUpdateMode)1));
			updnSDLimit.set_DecimalPlaces(1);
			updnSDLimit.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnSDLimit).set_Location(new Point(6, 62));
			updnSDLimit.set_Maximum(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnSDLimit.set_Minimum(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnSDLimit).set_Name("updnSDLimit");
			((Control)updnSDLimit).set_Size(new Size(43, 20));
			((Control)updnSDLimit).set_TabIndex(47);
			toolTip.SetToolTip((Control)(object)updnSDLimit, "When running the Monte Carlo trials, the artificial noise is limited to being less than this value");
			updnSDLimit.set_Value(Settings.Default.MonteCarloNoiseLimit);
			updnSDLimit.add_ValueChanged((EventHandler)updnSDLimit_ValueChanged);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(56, 17));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(98, 13));
			((Control)label31).set_TabIndex(44);
			((Control)label31).set_Text("# Monte Carlo trials");
			toolTip.SetToolTip((Control)(object)label31, "The larger the number or trials, the better the estimation of the uncertainties - but at a cost of slower response time.");
			panelAdjustRegions.set_BorderStyle((BorderStyle)1);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)label29);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)txtBeforeD);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)txtAfterD);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)txtAfterR);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)optBeforeD);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)optBeforeR);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)optAfterR);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)txtBeforeR);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)optAfterD);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)label26);
			((Control)panelAdjustRegions).get_Controls().Add((Control)(object)updnExtraPoints);
			((Control)panelAdjustRegions).set_Location(new Point(5, 3));
			((Control)panelAdjustRegions).set_Name("panelAdjustRegions");
			((Control)panelAdjustRegions).set_Size(new Size(142, 157));
			((Control)panelAdjustRegions).set_TabIndex(40);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(17, 5));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(87, 13));
			((Control)label29).set_TabIndex(39);
			((Control)label29).set_Text("Adjust regions");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(54, 21));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(85, 26));
			((Control)label26).set_TabIndex(31);
			((Control)label26).set_Text("# points outside \r\nof transition");
			label26.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)updnExtraPoints).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "PointsOutsideTransition", true, (DataSourceUpdateMode)1));
			((Control)updnExtraPoints).set_Location(new Point(6, 24));
			updnExtraPoints.set_Maximum(new decimal(new int[4] { 40, 0, 0, 0 }));
			updnExtraPoints.set_Minimum(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnExtraPoints).set_Name("updnExtraPoints");
			((Control)updnExtraPoints).set_Size(new Size(40, 20));
			((Control)updnExtraPoints).set_TabIndex(29);
			updnExtraPoints.set_Value(Settings.Default.PointsOutsideTransition);
			updnExtraPoints.add_ValueChanged((EventHandler)updnExtraPoints_ValueChanged);
			((Control)tabPage7).set_BackColor(Color.LightGreen);
			((Control)tabPage7).get_Controls().Add((Control)(object)panelTab5);
			tabPage7.set_Location(new Point(4, 22));
			((Control)tabPage7).set_Name("tabPage7");
			((Control)tabPage7).set_Size(new Size(892, 166));
			tabPage7.set_TabIndex(4);
			((Control)tabPage7).set_Text("6. Camera corrections && final times   ");
			tabPage7.set_UseVisualStyleBackColor(true);
			((Control)panelTab5).get_Controls().Add((Control)(object)lblEventIsMiss);
			((Control)panelTab5).get_Controls().Add((Control)(object)panelView);
			((Control)panelTab5).get_Controls().Add((Control)(object)grpSaveImages);
			((Control)panelTab5).get_Controls().Add((Control)(object)grpFinalResults);
			((Control)panelTab5).get_Controls().Add((Control)(object)grpCameraCorrections);
			((Control)panelTab5).set_Location(new Point(1, 1));
			((Control)panelTab5).set_Name("panelTab5");
			((Control)panelTab5).set_Size(new Size(889, 171));
			((Control)panelTab5).set_TabIndex(12);
			((Control)lblEventIsMiss).set_AutoSize(true);
			((Control)lblEventIsMiss).set_BackColor(Color.Aquamarine);
			((Control)lblEventIsMiss).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblEventIsMiss).set_ForeColor(Color.Red);
			((Control)lblEventIsMiss).set_Location(new Point(836, 53));
			((Control)lblEventIsMiss).set_Name("lblEventIsMiss");
			((Control)lblEventIsMiss).set_Size(new Size(126, 72));
			((Control)lblEventIsMiss).set_TabIndex(44);
			((Control)lblEventIsMiss).set_Text("Event set as\r\na Non-Event\r\n(see tab 4)");
			lblEventIsMiss.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lblEventIsMiss).set_Visible(false);
			((Control)panelView).get_Controls().Add((Control)(object)cmdReportLightCurve);
			((Control)panelView).get_Controls().Add((Control)(object)lblEventsToAnalyse);
			((Control)panelView).get_Controls().Add((Control)(object)label48);
			((Control)panelView).get_Controls().Add((Control)(object)cmdDisplayReport);
			((Control)panelView).get_Controls().Add((Control)(object)cmdSaveReport);
			((Control)panelView).get_Controls().Add((Control)(object)cmdSetMiss);
			((Control)panelView).set_Location(new Point(720, 1));
			((Control)panelView).set_Name("panelView");
			((Control)panelView).set_Size(new Size(124, 162));
			((Control)panelView).set_TabIndex(52);
			((Control)cmdReportLightCurve).set_BackColor(Color.Yellow);
			((ButtonBase)cmdReportLightCurve).set_FlatStyle((FlatStyle)0);
			((Control)cmdReportLightCurve).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReportLightCurve).set_ForeColor(Color.DarkRed);
			((Control)cmdReportLightCurve).set_Location(new Point(5, 119));
			((Control)cmdReportLightCurve).set_Name("cmdReportLightCurve");
			((Control)cmdReportLightCurve).set_Size(new Size(108, 39));
			((Control)cmdReportLightCurve).set_TabIndex(4);
			((Control)cmdReportLightCurve).set_Text("Report the\r\nLight curve");
			((ButtonBase)cmdReportLightCurve).set_UseVisualStyleBackColor(false);
			((Control)cmdReportLightCurve).add_Click((EventHandler)cmdReportLightCurve_Click);
			((Control)lblEventsToAnalyse).set_AutoSize(true);
			((Control)lblEventsToAnalyse).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblEventsToAnalyse).set_ForeColor(Color.DarkBlue);
			((Control)lblEventsToAnalyse).set_Location(new Point(1, 3));
			((Control)lblEventsToAnalyse).set_Name("lblEventsToAnalyse");
			((Control)lblEventsToAnalyse).set_Size(new Size(14, 13));
			((Control)lblEventsToAnalyse).set_TabIndex(0);
			((Control)lblEventsToAnalyse).set_Text("0");
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label48).set_ForeColor(Color.DarkBlue);
			((Control)label48).set_Location(new Point(13, 3));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(107, 13));
			((Control)label48).set_TabIndex(4);
			((Control)label48).set_Text("events to analyse");
			((Control)cmdDisplayReport).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDisplayReport).set_Location(new Point(5, 57));
			((Control)cmdDisplayReport).set_Name("cmdDisplayReport");
			((Control)cmdDisplayReport).set_Size(new Size(108, 26));
			((Control)cmdDisplayReport).set_TabIndex(2);
			((Control)cmdDisplayReport).set_Text("View Report");
			((ButtonBase)cmdDisplayReport).set_UseVisualStyleBackColor(true);
			((Control)cmdDisplayReport).add_Click((EventHandler)cmdDisplayReport_Click);
			((Control)cmdSaveReport).set_BackColor(Color.Aqua);
			((ButtonBase)cmdSaveReport).set_FlatStyle((FlatStyle)0);
			((Control)cmdSaveReport).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSaveReport).set_Location(new Point(5, 88));
			((Control)cmdSaveReport).set_Name("cmdSaveReport");
			((Control)cmdSaveReport).set_Size(new Size(108, 26));
			((Control)cmdSaveReport).set_TabIndex(3);
			((Control)cmdSaveReport).set_Text("Save Report");
			((ButtonBase)cmdSaveReport).set_UseVisualStyleBackColor(false);
			((Control)cmdSaveReport).add_Click((EventHandler)cmdSaveReport_Click);
			((Control)cmdSetMiss).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetMiss).set_Location(new Point(5, 17));
			((Control)cmdSetMiss).set_Name("cmdSetMiss");
			((Control)cmdSetMiss).set_Size(new Size(108, 37));
			((Control)cmdSetMiss).set_TabIndex(1);
			((Control)cmdSetMiss).set_Text("Set Event # 0 as\r\na non-event");
			((ButtonBase)cmdSetMiss).set_UseVisualStyleBackColor(true);
			((Control)cmdSetMiss).add_Click((EventHandler)cmdSetMiss_Click);
			((Control)grpSaveImages).get_Controls().Add((Control)(object)label80);
			((Control)grpSaveImages).get_Controls().Add((Control)(object)txtNameToAdd);
			((Control)grpSaveImages).get_Controls().Add((Control)(object)panel12);
			((Control)grpSaveImages).get_Controls().Add((Control)(object)cmdSaveMissAnalysis);
			((Control)grpSaveImages).get_Controls().Add((Control)(object)chkAddNameToSaves);
			((Control)grpSaveImages).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSaveImages).set_Location(new Point(587, 2));
			((Control)grpSaveImages).set_Name("grpSaveImages");
			((Control)grpSaveImages).set_Size(new Size(130, 155));
			((Control)grpSaveImages).set_TabIndex(51);
			grpSaveImages.set_TabStop(false);
			((Control)grpSaveImages).set_Text("Save AOTA images");
			((Control)label80).set_AutoSize(true);
			((Control)label80).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label80).set_Location(new Point(4, 138));
			((Control)label80).set_Name("label80");
			((Control)label80).set_Size(new Size(34, 13));
			((Control)label80).set_TabIndex(7);
			((Control)label80).set_Text("Name");
			((Control)txtNameToAdd).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AOTA_NameToAdd", true, (DataSourceUpdateMode)1));
			((Control)txtNameToAdd).set_Location(new Point(39, 134));
			((Control)txtNameToAdd).set_Name("txtNameToAdd");
			((Control)txtNameToAdd).set_Size(new Size(86, 20));
			((Control)txtNameToAdd).set_TabIndex(5);
			((Control)txtNameToAdd).set_Text(Settings.Default.AOTA_NameToAdd);
			panel12.set_BorderStyle((BorderStyle)2);
			((Control)panel12).get_Controls().Add((Control)(object)lblMonitorScale);
			((Control)panel12).get_Controls().Add((Control)(object)cmdSaveResults);
			((Control)panel12).set_Location(new Point(7, 16));
			((Control)panel12).set_Name("panel12");
			((Control)panel12).set_Size(new Size(117, 58));
			((Control)panel12).set_TabIndex(4);
			((Control)lblMonitorScale).set_AutoSize(true);
			((Control)lblMonitorScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMonitorScale).set_ForeColor(Color.DarkRed);
			((Control)lblMonitorScale).set_Location(new Point(7, 41));
			((Control)lblMonitorScale).set_Name("lblMonitorScale");
			((Control)lblMonitorScale).set_Size(new Size(102, 13));
			((Control)lblMonitorScale).set_TabIndex(3);
			((Control)lblMonitorScale).set_Text("Monitor scale: 100%");
			toolTip.SetToolTip((Control)(object)lblMonitorScale, "To adjust the Monitor scale setting so\r\nthat the saved image is of the correct\r\nsize and location, use the menu item\r\n\r\nFile... Save image form at current tab...\r\n");
			((Control)cmdSaveResults).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSaveResults).set_Location(new Point(1, 1));
			((Control)cmdSaveResults).set_Name("cmdSaveResults");
			((Control)cmdSaveResults).set_Size(new Size(111, 37));
			((Control)cmdSaveResults).set_TabIndex(0);
			((Control)cmdSaveResults).set_Text("Save form at Tab 5\r\nfor Event # 1");
			((ButtonBase)cmdSaveResults).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveResults).add_Click((EventHandler)cmdSaveResults_Click);
			((Control)cmdSaveMissAnalysis).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSaveMissAnalysis).set_Location(new Point(10, 78));
			((Control)cmdSaveMissAnalysis).set_Name("cmdSaveMissAnalysis");
			((Control)cmdSaveMissAnalysis).set_Size(new Size(111, 37));
			((Control)cmdSaveMissAnalysis).set_TabIndex(1);
			((Control)cmdSaveMissAnalysis).set_Text("Save Tab 4 image\r\nof plot analysis");
			((ButtonBase)cmdSaveMissAnalysis).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveMissAnalysis).add_Click((EventHandler)cmdSaveMissAnalysis_Click);
			((Control)chkAddNameToSaves).set_AutoSize(true);
			chkAddNameToSaves.set_CheckAlign(ContentAlignment.TopLeft);
			chkAddNameToSaves.set_Checked(Settings.Default.AOTA_AddName);
			((Control)chkAddNameToSaves).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AOTA_AddName", true, (DataSourceUpdateMode)1));
			((Control)chkAddNameToSaves).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAddNameToSaves).set_Location(new Point(6, 119));
			((Control)chkAddNameToSaves).set_Name("chkAddNameToSaves");
			((Control)chkAddNameToSaves).set_Size(new Size(119, 17));
			((Control)chkAddNameToSaves).set_TabIndex(6);
			((Control)chkAddNameToSaves).set_Text("Add name to Saves");
			((ButtonBase)chkAddNameToSaves).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)chkAddNameToSaves).set_UseVisualStyleBackColor(true);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)cmdVerifyTimes);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lblSNR);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lblDFrameFinal);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lblRtransition);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label53);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lblDtransition);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label52);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label45);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label51);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label44);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label50);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label43);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lbl_UT_D);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label6);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lbl_UT_R);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lbl_UTuncert_D);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lblRFrameFinal);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lbl_UTuncert_R);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)lblTimeSource);
			((Control)grpFinalResults).get_Controls().Add((Control)(object)label5);
			((Control)grpFinalResults).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpFinalResults).set_Location(new Point(359, 2));
			((Control)grpFinalResults).set_Name("grpFinalResults");
			((Control)grpFinalResults).set_Size(new Size(223, 155));
			((Control)grpFinalResults).set_TabIndex(42);
			grpFinalResults.set_TabStop(false);
			((Control)grpFinalResults).set_Text("Final results");
			((Control)cmdVerifyTimes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdVerifyTimes).set_Location(new Point(161, 16));
			((Control)cmdVerifyTimes).set_Name("cmdVerifyTimes");
			((Control)cmdVerifyTimes).set_Size(new Size(51, 22));
			((Control)cmdVerifyTimes).set_TabIndex(42);
			((Control)cmdVerifyTimes).set_Text("Verify");
			((ButtonBase)cmdVerifyTimes).set_UseVisualStyleBackColor(true);
			((Control)cmdVerifyTimes).add_Click((EventHandler)cmdVerifyTimes_Click);
			((Control)lblSNR).set_AutoSize(true);
			((Control)lblSNR).set_ForeColor(Color.Red);
			((Control)lblSNR).set_Location(new Point(133, 135));
			((Control)lblSNR).set_Name("lblSNR");
			((Control)lblSNR).set_Size(new Size(64, 13));
			((Control)lblSNR).set_TabIndex(2);
			((Control)lblSNR).set_Text("SnR = 0.0");
			((Control)lblDFrameFinal).set_AutoSize(true);
			((Control)lblDFrameFinal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDFrameFinal).set_Location(new Point(29, 39));
			((Control)lblDFrameFinal).set_Name("lblDFrameFinal");
			((Control)lblDFrameFinal).set_Size(new Size(48, 13));
			((Control)lblDFrameFinal).set_TabIndex(34);
			((Control)lblDFrameFinal).set_Text("at frame:");
			((Control)lblRtransition).set_AutoSize(true);
			((Control)lblRtransition).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRtransition).set_Location(new Point(71, 121));
			((Control)lblRtransition).set_Name("lblRtransition");
			((Control)lblRtransition).set_Size(new Size(47, 13));
			((Control)lblRtransition).set_TabIndex(41);
			((Control)lblRtransition).set_Text("1 frames");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(8, 54));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(60, 13));
			((Control)label53).set_TabIndex(1);
			((Control)label53).set_Text("UTC of D");
			((Control)lblDtransition).set_AutoSize(true);
			((Control)lblDtransition).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDtransition).set_Location(new Point(71, 69));
			((Control)lblDtransition).set_Name("lblDtransition");
			((Control)lblDtransition).set_Size(new Size(47, 13));
			((Control)lblDtransition).set_TabIndex(40);
			((Control)lblDtransition).set_Text("1 frames");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label52).set_Location(new Point(8, 106));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(60, 13));
			((Control)label52).set_TabIndex(2);
			((Control)label52).set_Text("UTC of R");
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(8, 121));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(63, 13));
			((Control)label45).set_TabIndex(39);
			((Control)label45).set_Text("Transition");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Location(new Point(160, 55));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(14, 13));
			((Control)label51).set_TabIndex(3);
			((Control)label51).set_Text("±");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(8, 69));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(63, 13));
			((Control)label44).set_TabIndex(38);
			((Control)label44).set_Text("Transition");
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Location(new Point(160, 107));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(14, 13));
			((Control)label50).set_TabIndex(4);
			((Control)label50).set_Text("±");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(8, 91));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(16, 13));
			((Control)label43).set_TabIndex(37);
			((Control)label43).set_Text("R");
			((Control)lbl_UT_D).set_AutoSize(true);
			((Control)lbl_UT_D).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl_UT_D).set_Location(new Point(72, 54));
			((Control)lbl_UT_D).set_Name("lbl_UT_D");
			((Control)lbl_UT_D).set_Size(new Size(91, 14));
			((Control)lbl_UT_D).set_TabIndex(5);
			((Control)lbl_UT_D).set_Text("hh mm ss.sss");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(8, 39));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(16, 13));
			((Control)label6).set_TabIndex(36);
			((Control)label6).set_Text("D");
			((Control)lbl_UT_R).set_AutoSize(true);
			((Control)lbl_UT_R).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl_UT_R).set_Location(new Point(72, 106));
			((Control)lbl_UT_R).set_Name("lbl_UT_R");
			((Control)lbl_UT_R).set_Size(new Size(91, 14));
			((Control)lbl_UT_R).set_TabIndex(6);
			((Control)lbl_UT_R).set_Text("hh mm ss.sss");
			((Control)lbl_UTuncert_D).set_AutoSize(true);
			((Control)lbl_UTuncert_D).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl_UTuncert_D).set_Location(new Point(173, 54));
			((Control)lbl_UTuncert_D).set_Name("lbl_UTuncert_D");
			((Control)lbl_UTuncert_D).set_Size(new Size(49, 14));
			((Control)lbl_UTuncert_D).set_TabIndex(7);
			((Control)lbl_UTuncert_D).set_Text("ss.sss");
			((Control)lblRFrameFinal).set_AutoSize(true);
			((Control)lblRFrameFinal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRFrameFinal).set_Location(new Point(29, 91));
			((Control)lblRFrameFinal).set_Name("lblRFrameFinal");
			((Control)lblRFrameFinal).set_Size(new Size(48, 13));
			((Control)lblRFrameFinal).set_TabIndex(35);
			((Control)lblRFrameFinal).set_Text("at frame:");
			((Control)lbl_UTuncert_R).set_AutoSize(true);
			((Control)lbl_UTuncert_R).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl_UTuncert_R).set_Location(new Point(173, 106));
			((Control)lbl_UTuncert_R).set_Name("lbl_UTuncert_R");
			((Control)lbl_UTuncert_R).set_Size(new Size(49, 14));
			((Control)lbl_UTuncert_R).set_TabIndex(8);
			((Control)lbl_UTuncert_R).set_Text("ss.sss");
			((Control)lblTimeSource).set_AutoSize(true);
			((Control)lblTimeSource).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTimeSource).set_Location(new Point(95, 20));
			((Control)lblTimeSource).set_Name("lblTimeSource");
			((Control)lblTimeSource).set_Size(new Size(42, 13));
			((Control)lblTimeSource).set_TabIndex(16);
			((Control)lblTimeSource).set_Text("Manual");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(8, 19));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(86, 13));
			((Control)label5).set_TabIndex(0);
			((Control)label5).set_Text("Time Source :");
			((Control)grpCameraCorrections).get_Controls().Add((Control)(object)lblTangraCameraCorrns);
			((Control)grpCameraCorrections).get_Controls().Add((Control)(object)panel5);
			((Control)grpCameraCorrections).get_Controls().Add((Control)(object)lblTimeDelaySecs);
			((Control)grpCameraCorrections).get_Controls().Add((Control)(object)panel2);
			((Control)grpCameraCorrections).get_Controls().Add((Control)(object)panel1);
			((Control)grpCameraCorrections).get_Controls().Add((Control)(object)label58);
			((Control)grpCameraCorrections).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpCameraCorrections).set_Location(new Point(7, 2));
			((Control)grpCameraCorrections).set_Name("grpCameraCorrections");
			((Control)grpCameraCorrections).set_Size(new Size(348, 150));
			((Control)grpCameraCorrections).set_TabIndex(15);
			grpCameraCorrections.set_TabStop(false);
			((Control)grpCameraCorrections).set_Text("Camera corrections");
			((Control)lblTangraCameraCorrns).set_AutoSize(true);
			((Control)lblTangraCameraCorrns).set_BackColor(Color.Aquamarine);
			((Control)lblTangraCameraCorrns).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblTangraCameraCorrns).set_ForeColor(Color.DarkRed);
			((Control)lblTangraCameraCorrns).set_Location(new Point(32, 100));
			((Control)lblTangraCameraCorrns).set_Name("lblTangraCameraCorrns");
			((Control)lblTangraCameraCorrns).set_Size(new Size(275, 15));
			((Control)lblTangraCameraCorrns).set_TabIndex(2);
			((Control)lblTangraCameraCorrns).set_Text("Camera corrections applied within Tangra");
			((Control)lblTangraCameraCorrns).set_Visible(false);
			panel5.set_BorderStyle((BorderStyle)1);
			((Control)panel5).get_Controls().Add((Control)(object)lblSetNTSCorPAL);
			((Control)panel5).get_Controls().Add((Control)(object)optSystemNotKnown);
			((Control)panel5).get_Controls().Add((Control)(object)optSystemOther);
			((Control)panel5).get_Controls().Add((Control)(object)label59);
			((Control)panel5).get_Controls().Add((Control)(object)optSystemNTSC);
			((Control)panel5).get_Controls().Add((Control)(object)optSystemPAL);
			((Control)panel5).set_Location(new Point(178, 13));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(166, 77));
			((Control)panel5).set_TabIndex(3);
			((Control)lblSetNTSCorPAL).set_AutoSize(true);
			((Control)lblSetNTSCorPAL).set_BackColor(Color.Yellow);
			((Control)lblSetNTSCorPAL).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSetNTSCorPAL).set_ForeColor(Color.DarkRed);
			((Control)lblSetNTSCorPAL).set_Location(new Point(19, 58));
			((Control)lblSetNTSCorPAL).set_Name("lblSetNTSCorPAL");
			((Control)lblSetNTSCorPAL).set_Size(new Size(126, 15));
			((Control)lblSetNTSCorPAL).set_TabIndex(4);
			((Control)lblSetNTSCorPAL).set_Text("Set NTSC or PAL !!");
			((Control)optSystemNotKnown).set_AutoSize(true);
			optSystemNotKnown.set_CheckAlign(ContentAlignment.TopCenter);
			optSystemNotKnown.set_Checked(true);
			((Control)optSystemNotKnown).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSystemNotKnown).set_Location(new Point(111, 14));
			((Control)optSystemNotKnown).set_Name("optSystemNotKnown");
			((Control)optSystemNotKnown).set_Size(new Size(57, 30));
			((Control)optSystemNotKnown).set_TabIndex(3);
			optSystemNotKnown.set_TabStop(true);
			((Control)optSystemNotKnown).set_Text("Unknown");
			((ButtonBase)optSystemNotKnown).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)optSystemNotKnown).set_UseVisualStyleBackColor(true);
			optSystemNotKnown.add_CheckedChanged((EventHandler)optSystemNotKnown_CheckedChanged);
			((Control)optSystemOther).set_AutoSize(true);
			optSystemOther.set_CheckAlign(ContentAlignment.TopCenter);
			((Control)optSystemOther).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSystemOther).set_Location(new Point(67, 14));
			((Control)optSystemOther).set_Name("optSystemOther");
			((Control)optSystemOther).set_Size(new Size(63, 56));
			((Control)optSystemOther).set_TabIndex(2);
			((Control)optSystemOther).set_Text("Other \r\n(ADVS\r\nAAV, SER)");
			((ButtonBase)optSystemOther).set_UseVisualStyleBackColor(true);
			optSystemOther.add_CheckedChanged((EventHandler)optSystemOther_CheckedChanged);
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label59).set_ForeColor(Color.DarkBlue);
			((Control)label59).set_Location(new Point(37, 1));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(81, 13));
			((Control)label59).set_TabIndex(0);
			((Control)label59).set_Text("Video system");
			((Control)optSystemNTSC).set_AutoSize(true);
			optSystemNTSC.set_CheckAlign(ContentAlignment.TopCenter);
			((Control)optSystemNTSC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSystemNTSC).set_Location(new Point(0, 14));
			((Control)optSystemNTSC).set_Name("optSystemNTSC");
			((Control)optSystemNTSC).set_Size(new Size(40, 30));
			((Control)optSystemNTSC).set_TabIndex(1);
			((Control)optSystemNTSC).set_Text("NTSC");
			((ButtonBase)optSystemNTSC).set_UseVisualStyleBackColor(true);
			optSystemNTSC.add_CheckedChanged((EventHandler)optSystemNTSC_CheckedChanged);
			((Control)optSystemPAL).set_AutoSize(true);
			optSystemPAL.set_CheckAlign(ContentAlignment.TopCenter);
			((Control)optSystemPAL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSystemPAL).set_Location(new Point(38, 14));
			((Control)optSystemPAL).set_Name("optSystemPAL");
			((Control)optSystemPAL).set_Size(new Size(31, 30));
			((Control)optSystemPAL).set_TabIndex(1);
			((Control)optSystemPAL).set_Text("PAL");
			((ButtonBase)optSystemPAL).set_UseVisualStyleBackColor(true);
			optSystemPAL.add_CheckedChanged((EventHandler)optSystemPAL_CheckedChanged);
			((Control)lblTimeDelaySecs).set_AutoSize(true);
			((Control)lblTimeDelaySecs).set_Location(new Point(228, 136));
			((Control)lblTimeDelaySecs).set_Name("lblTimeDelaySecs");
			((Control)lblTimeDelaySecs).set_Size(new Size(69, 13));
			((Control)lblTimeDelaySecs).set_TabIndex(5);
			((Control)lblTimeDelaySecs).set_Text("0.000 secs");
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)label56);
			((Control)panel2).get_Controls().Add((Control)(object)cmbCamera);
			((Control)panel2).get_Controls().Add((Control)(object)lblCameraDelayFrames);
			((Control)panel2).get_Controls().Add((Control)(object)label42);
			((Control)panel2).get_Controls().Add((Control)(object)lblSelectCamera);
			((Control)panel2).set_Location(new Point(3, 13));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(173, 77));
			((Control)panel2).set_TabIndex(0);
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label56).set_ForeColor(Color.DarkBlue);
			((Control)label56).set_Location(new Point(43, 2));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(84, 13));
			((Control)label56).set_TabIndex(0);
			((Control)label56).set_Text("Video camera");
			((Control)cmbCamera).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbCamera).set_FormattingEnabled(true);
			cmbCamera.get_Items().AddRange(new object[14]
			{
				"Mintron 12V1C-EX  NTSC", "Mintron 12V1C-EX  PAL", "PC164C-EX2   NTSC", "PC165DNR NTSC", "SCB-2000N NTSC", "SK-1004XC/50   PAL", "WAT-120N   NTSC", "WAT-120N   PAL", "WAT-120N+   PAL", "WAT-120N+  NTSC",
				"WAT-902H2 Ultimate  NTSC", "WAT-902H2 Ultimate  PAL", "WAT-910HX  NTSC", "WAT-910HX  PAL"
			});
			((Control)cmbCamera).set_Location(new Point(5, 18));
			((Control)cmbCamera).set_Name("cmbCamera");
			((Control)cmbCamera).set_Size(new Size(162, 21));
			((Control)cmbCamera).set_TabIndex(1);
			cmbCamera.add_SelectedIndexChanged((EventHandler)cmbCamera_SelectedIndexChanged);
			((Control)cmbCamera).add_Click((EventHandler)cmbCamera_Click);
			((Control)cmbCamera).add_Enter((EventHandler)cmbCamera_Enter);
			((Control)cmbCamera).add_Leave((EventHandler)cmbCamera_Leave);
			((Control)lblCameraDelayFrames).set_AutoSize(true);
			((Control)lblCameraDelayFrames).set_Location(new Point(80, 57));
			((Control)lblCameraDelayFrames).set_Name("lblCameraDelayFrames");
			((Control)lblCameraDelayFrames).set_Size(new Size(66, 13));
			((Control)lblCameraDelayFrames).set_TabIndex(4);
			((Control)lblCameraDelayFrames).set_Text("0.0 frames");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(6, 57));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(74, 13));
			((Control)label42).set_TabIndex(3);
			((Control)label42).set_Text("Camera delay:");
			((Control)lblSelectCamera).set_AutoSize(true);
			((Control)lblSelectCamera).set_BackColor(Color.Yellow);
			((Control)lblSelectCamera).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSelectCamera).set_ForeColor(Color.DarkRed);
			((Control)lblSelectCamera).set_Location(new Point(35, 40));
			((Control)lblSelectCamera).set_Name("lblSelectCamera");
			((Control)lblSelectCamera).set_Size(new Size(100, 13));
			((Control)lblSelectCamera).set_TabIndex(2);
			((Control)lblSelectCamera).set_Text("Select camera !!");
			((Control)lblSelectCamera).set_Visible(false);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)lblIntegrationFrameDelay);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)cmbFrames);
			((Control)panel1).get_Controls().Add((Control)(object)label55);
			((Control)panel1).get_Controls().Add((Control)(object)lblSetIntegn);
			((Control)panel1).set_Location(new Point(3, 96));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(173, 63));
			((Control)panel1).set_TabIndex(1);
			((Control)lblIntegrationFrameDelay).set_AutoSize(true);
			((Control)lblIntegrationFrameDelay).set_Location(new Point(88, 41));
			((Control)lblIntegrationFrameDelay).set_Name("lblIntegrationFrameDelay");
			((Control)lblIntegrationFrameDelay).set_Size(new Size(55, 13));
			((Control)lblIntegrationFrameDelay).set_TabIndex(4);
			((Control)lblIntegrationFrameDelay).set_Text("0 frames");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(6, 41));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(82, 13));
			((Control)label7).set_TabIndex(3);
			((Control)label7).set_Text("Exposure delay:");
			((Control)cmbFrames).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbFrames).set_FormattingEnabled(true);
			cmbFrames.get_Items().AddRange(new object[16]
			{
				"0", "1", "2", "4", "6", "8", "12", "16", "24", "32",
				"48", "64", "96", "128", "256", "512"
			});
			((Control)cmbFrames).set_Location(new Point(99, 7));
			((Control)cmbFrames).set_Name("cmbFrames");
			((Control)cmbFrames).set_Size(new Size(47, 21));
			((Control)cmbFrames).set_TabIndex(1);
			cmbFrames.add_SelectedIndexChanged((EventHandler)cmbFrames_SelectedIndexChanged);
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(6, 2));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(88, 26));
			((Control)label55).set_TabIndex(0);
			((Control)label55).set_Text("Number of video\r\nframes integrated");
			((Control)lblSetIntegn).set_AutoSize(true);
			((Control)lblSetIntegn).set_BackColor(Color.Yellow);
			((Control)lblSetIntegn).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSetIntegn).set_ForeColor(Color.DarkRed);
			((Control)lblSetIntegn).set_Location(new Point(7, 30));
			((Control)lblSetIntegn).set_Name("lblSetIntegn");
			((Control)lblSetIntegn).set_Size(new Size(79, 13));
			((Control)lblSetIntegn).set_TabIndex(2);
			((Control)lblSetIntegn).set_Text("Set frames !!");
			((Control)lblSetIntegn).set_Visible(false);
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label58).set_Location(new Point(192, 103));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(135, 26));
			((Control)label58).set_TabIndex(4);
			((Control)label58).set_Text("Time difference from video \r\nstamp to start of exposure");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(50, 5));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(14, 13));
			((Control)label39).set_TabIndex(13);
			((Control)label39).set_Text("↕");
			updnVerticalScaleAdjustment.set_DecimalPlaces(2);
			((Control)updnVerticalScaleAdjustment).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnVerticalScaleAdjustment.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnVerticalScaleAdjustment).set_Location(new Point(63, 3));
			updnVerticalScaleAdjustment.set_Maximum(new decimal(new int[4] { 15, 0, 0, 65536 }));
			updnVerticalScaleAdjustment.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnVerticalScaleAdjustment).set_Name("updnVerticalScaleAdjustment");
			((Control)updnVerticalScaleAdjustment).set_Size(new Size(43, 20));
			((Control)updnVerticalScaleAdjustment).set_TabIndex(12);
			updnVerticalScaleAdjustment.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnVerticalScaleAdjustment.add_ValueChanged((EventHandler)updnVerticalScaleAdjustment_ValueChanged);
			((Control)cmdTest).set_Location(new Point(982, 5));
			((Control)cmdTest).set_Name("cmdTest");
			((Control)cmdTest).set_Size(new Size(44, 28));
			((Control)cmdTest).set_TabIndex(13);
			((Control)cmdTest).set_Text("Test");
			((ButtonBase)cmdTest).set_UseVisualStyleBackColor(true);
			((Control)cmdTest).set_Visible(false);
			((Control)cmdTest).add_Click((EventHandler)cmdTest_Click);
			((Control)cmd_x10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x10).set_Location(new Point(53, 22));
			((Control)cmd_x10).set_Name("cmd_x10");
			((Control)cmd_x10).set_Size(new Size(27, 20));
			((Control)cmd_x10).set_TabIndex(10);
			((Control)cmd_x10).set_Text("10");
			((ButtonBase)cmd_x10).set_UseVisualStyleBackColor(true);
			((Control)cmd_x10).add_Click((EventHandler)cmd_x10_Click);
			((Control)cmd_x15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x15).set_Location(new Point(87, 22));
			((Control)cmd_x15).set_Name("cmd_x15");
			((Control)cmd_x15).set_Size(new Size(27, 20));
			((Control)cmd_x15).set_TabIndex(9);
			((Control)cmd_x15).set_Text("15");
			((ButtonBase)cmd_x15).set_UseVisualStyleBackColor(true);
			((Control)cmd_x15).add_Click((EventHandler)cmd_x15_Click);
			((Control)cmd_x5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x5).set_Location(new Point(28, 22));
			((Control)cmd_x5).set_Name("cmd_x5");
			((Control)cmd_x5).set_Size(new Size(18, 20));
			((Control)cmd_x5).set_TabIndex(8);
			((Control)cmd_x5).set_Text("5");
			((ButtonBase)cmd_x5).set_UseVisualStyleBackColor(true);
			((Control)cmd_x5).add_Click((EventHandler)cmd_x5_Click);
			((Control)cmd_x1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x1).set_Location(new Point(2, 22));
			((Control)cmd_x1).set_Name("cmd_x1");
			((Control)cmd_x1).set_Size(new Size(19, 20));
			((Control)cmd_x1).set_TabIndex(7);
			((Control)cmd_x1).set_Text("1");
			((ButtonBase)cmd_x1).set_UseVisualStyleBackColor(true);
			((Control)cmd_x1).add_Click((EventHandler)cmd_x1_Click);
			((Control)updnScale).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "PlotScale", true, (DataSourceUpdateMode)1));
			updnScale.set_DecimalPlaces(1);
			((Control)updnScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnScale.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnScale).set_Location(new Point(64, 1));
			updnScale.set_Maximum(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnScale.set_Minimum(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnScale).set_Name("updnScale");
			((Control)updnScale).set_Size(new Size(43, 20));
			((Control)updnScale).set_TabIndex(6);
			updnScale.set_Value(Settings.Default.PlotScale);
			updnScale.add_ValueChanged((EventHandler)updnScale_ValueChanged);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(45, 3));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(19, 13));
			((Control)label41).set_TabIndex(11);
			((Control)label41).set_Text("↔");
			((Control)panelComp).set_BackColor(Color.FloralWhite);
			panelComp.set_BorderStyle((BorderStyle)2);
			((Control)panelComp).get_Controls().Add((Control)(object)chkScaleComparisons);
			((Control)panelComp).get_Controls().Add((Control)(object)cmbPlotAverage);
			((Control)panelComp).get_Controls().Add((Control)(object)chkStarPoints);
			((Control)panelComp).get_Controls().Add((Control)(object)label57);
			((Control)panelComp).get_Controls().Add((Control)(object)chkComp3);
			((Control)panelComp).get_Controls().Add((Control)(object)chkComp2);
			((Control)panelComp).get_Controls().Add((Control)(object)chkComp1);
			((Control)panelComp).get_Controls().Add((Control)(object)chkTarget);
			((Control)panelComp).get_Controls().Add((Control)(object)chkShowBackground);
			((Control)panelComp).set_Location(new Point(1, 408));
			((Control)panelComp).set_Name("panelComp");
			((Control)panelComp).set_Size(new Size(121, 121));
			((Control)panelComp).set_TabIndex(15);
			((Control)chkScaleComparisons).set_AutoSize(true);
			((Control)chkScaleComparisons).set_BackColor(Color.RoyalBlue);
			chkScaleComparisons.set_Checked(true);
			chkScaleComparisons.set_CheckState((CheckState)1);
			((Control)chkScaleComparisons).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkScaleComparisons).set_ForeColor(Color.Yellow);
			((Control)chkScaleComparisons).set_Location(new Point(1, 99));
			((Control)chkScaleComparisons).set_Name("chkScaleComparisons");
			((Control)chkScaleComparisons).set_Size(new Size(120, 17));
			((Control)chkScaleComparisons).set_TabIndex(19);
			((Control)chkScaleComparisons).set_Text("Comparisons scaled");
			((ButtonBase)chkScaleComparisons).set_UseVisualStyleBackColor(false);
			chkScaleComparisons.add_CheckedChanged((EventHandler)chkScaleComparisons_CheckedChanged);
			((ListControl)cmbPlotAverage).set_FormattingEnabled(true);
			cmbPlotAverage.get_Items().AddRange(new object[8] { "0", "3", "5", "7", "9", "15", "25", "35" });
			((Control)cmbPlotAverage).set_Location(new Point(5, 2));
			((Control)cmbPlotAverage).set_Name("cmbPlotAverage");
			((Control)cmbPlotAverage).set_Size(new Size(36, 21));
			((Control)cmbPlotAverage).set_TabIndex(15);
			cmbPlotAverage.add_SelectedIndexChanged((EventHandler)cmbPlotAverage_SelectedIndexChanged);
			((Control)chkStarPoints).set_AutoSize(true);
			chkStarPoints.set_Checked(true);
			chkStarPoints.set_CheckState((CheckState)1);
			((Control)chkStarPoints).set_Location(new Point(4, 25));
			((Control)chkStarPoints).set_Name("chkStarPoints");
			((Control)chkStarPoints).set_Size(new Size(15, 14));
			((Control)chkStarPoints).set_TabIndex(18);
			((ButtonBase)chkStarPoints).set_UseVisualStyleBackColor(true);
			chkStarPoints.add_CheckedChanged((EventHandler)chkStarPoints_CheckedChanged);
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_ForeColor(Color.Fuchsia);
			((Control)label57).set_Location(new Point(38, 5));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(78, 13));
			((Control)label57).set_TabIndex(16);
			((Control)label57).set_Text("- point average");
			((Control)chkComp3).set_Anchor((AnchorStyles)10);
			((Control)chkComp3).set_AutoSize(true);
			((Control)chkComp3).set_BackColor(Color.FromArgb(255, 100, 100));
			chkComp3.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkComp3).set_ForeColor(Color.Black);
			((Control)chkComp3).set_Location(new Point(6, 69));
			((Control)chkComp3).set_Name("chkComp3");
			((Control)chkComp3).set_Size(new Size(110, 17));
			((Control)chkComp3).set_TabIndex(11);
			((Control)chkComp3).set_Text("Comparison star 3");
			((ButtonBase)chkComp3).set_UseVisualStyleBackColor(false);
			chkComp3.add_CheckedChanged((EventHandler)chkComp3_CheckedChanged);
			((Control)chkTarget).set_Anchor((AnchorStyles)10);
			((Control)chkTarget).set_AutoSize(true);
			chkTarget.set_CheckAlign(ContentAlignment.MiddleRight);
			chkTarget.set_Checked(Settings.Default.AOTA_LinesForTarget);
			chkTarget.set_CheckState((CheckState)1);
			((Control)chkTarget).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AOTA_LinesForTarget", true, (DataSourceUpdateMode)1));
			((Control)chkTarget).set_ForeColor(Color.Blue);
			((Control)chkTarget).set_Location(new Point(16, 24));
			((Control)chkTarget).set_Name("chkTarget");
			((Control)chkTarget).set_Size(new Size(100, 17));
			((Control)chkTarget).set_TabIndex(17);
			((Control)chkTarget).set_Text("pts   Target star");
			((ButtonBase)chkTarget).set_UseVisualStyleBackColor(true);
			chkTarget.add_CheckedChanged((EventHandler)chkTarget_CheckedChanged);
			((Control)chkShowBackground).set_Anchor((AnchorStyles)10);
			((Control)chkShowBackground).set_AutoSize(true);
			((Control)chkShowBackground).set_BackColor(Color.Peru);
			chkShowBackground.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkShowBackground).set_ForeColor(Color.Black);
			((Control)chkShowBackground).set_Location(new Point(32, 84));
			((Control)chkShowBackground).set_Name("chkShowBackground");
			((Control)chkShowBackground).set_Size(new Size(84, 17));
			((Control)chkShowBackground).set_TabIndex(10);
			((Control)chkShowBackground).set_Text("Background");
			((ButtonBase)chkShowBackground).set_UseVisualStyleBackColor(false);
			chkShowBackground.add_CheckedChanged((EventHandler)chkShowBackground_CheckedChanged);
			toolTip.set_AutomaticDelay(1000);
			toolTip.set_AutoPopDelay(5000);
			toolTip.set_InitialDelay(100);
			toolTip.set_ReshowDelay(200);
			toolTip.set_ToolTipIcon((ToolTipIcon)1);
			toolTip.set_ToolTipTitle("Frame | Height | Time");
			((Control)lblRunningInTangra).set_AutoSize(true);
			((Control)lblRunningInTangra).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblRunningInTangra).set_ForeColor(Color.MediumOrchid);
			((Control)lblRunningInTangra).set_Location(new Point(38, 25));
			((Control)lblRunningInTangra).set_Name("lblRunningInTangra");
			((Control)lblRunningInTangra).set_Size(new Size(214, 13));
			((Control)lblRunningInTangra).set_TabIndex(16);
			((Control)lblRunningInTangra).set_Text("Right-click  -  display frame in Tangra");
			((Control)lblRunningInTangra).set_Visible(false);
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label60).set_ForeColor(Color.Tomato);
			((Control)label60).set_Location(new Point(617, 25));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(294, 13));
			((Control)label60).set_TabIndex(17);
			((Control)label60).set_Text("CTRL + Right-click  -  toggle validity of a data point");
			((Control)pnlScale).set_BackColor(Color.SeaShell);
			pnlScale.set_BorderStyle((BorderStyle)2);
			((Control)pnlScale).get_Controls().Add((Control)(object)panel11);
			((Control)pnlScale).get_Controls().Add((Control)(object)label76);
			((Control)pnlScale).get_Controls().Add((Control)(object)cmd_x15);
			((Control)pnlScale).get_Controls().Add((Control)(object)cmd_x10);
			((Control)pnlScale).get_Controls().Add((Control)(object)updnScale);
			((Control)pnlScale).get_Controls().Add((Control)(object)label41);
			((Control)pnlScale).get_Controls().Add((Control)(object)cmd_x1);
			((Control)pnlScale).get_Controls().Add((Control)(object)cmd_x5);
			((Control)pnlScale).set_Location(new Point(1, 333));
			((Control)pnlScale).set_Name("pnlScale");
			((Control)pnlScale).set_Size(new Size(121, 71));
			((Control)pnlScale).set_TabIndex(18);
			((Control)panel11).set_BackColor(Color.MistyRose);
			((Control)panel11).get_Controls().Add((Control)(object)label77);
			((Control)panel11).get_Controls().Add((Control)(object)updnVerticalScaleAdjustment);
			((Control)panel11).get_Controls().Add((Control)(object)label39);
			((Control)panel11).set_Location(new Point(-1, 43));
			((Control)panel11).set_Name("panel11");
			((Control)panel11).set_Size(new Size(119, 25));
			((Control)panel11).set_TabIndex(17);
			((Control)label77).set_AutoSize(true);
			((Control)label77).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label77).set_Location(new Point(13, 6));
			((Control)label77).set_Name("label77");
			((Control)label77).set_Size(new Size(39, 13));
			((Control)label77).set_TabIndex(16);
			((Control)label77).set_Text("Scale");
			((Control)label76).set_AutoSize(true);
			((Control)label76).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label76).set_Location(new Point(10, 3));
			((Control)label76).set_Name("label76");
			((Control)label76).set_Size(new Size(39, 13));
			((Control)label76).set_TabIndex(15);
			((Control)label76).set_Text("Scale");
			((Control)label79).set_AutoSize(true);
			((Control)label79).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label79).set_ForeColor(Color.SeaGreen);
			((Control)label79).set_Location(new Point(326, 25));
			((Control)label79).set_Name("label79");
			((Control)label79).set_Size(new Size(217, 13));
			((Control)label79).set_TabIndex(19);
			((Control)label79).set_Text("ALT + Right-click  -  set full light level");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(Settings.Default.FormSize);
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)pnlScale);
			((Control)this).get_Controls().Add((Control)(object)panelComp);
			((Control)this).get_Controls().Add((Control)(object)cmdTest);
			((Control)this).get_Controls().Add((Control)(object)tabPlot);
			((Control)this).get_Controls().Add((Control)(object)panelPlot);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label60);
			((Control)this).get_Controls().Add((Control)(object)label79);
			((Control)this).get_Controls().Add((Control)(object)lblRunningInTangra);
			((Control)this).get_DataBindings().Add(new Binding("ClientSize", (object)Settings.Default, "FormSize", true, (DataSourceUpdateMode)1));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_MinimumSize(new Size(1030, 400));
			((Control)this).set_Name("PlotForm");
			((Form)this).set_StartPosition((FormStartPosition)4);
			((Control)this).set_Text("Plot Form");
			((Form)this).add_FormClosing(new FormClosingEventHandler(PlotForm_FormClosing));
			((Form)this).add_Load((EventHandler)PlotForm_Load);
			((Form)this).add_Shown((EventHandler)PlotForm_Shown);
			((Control)this).add_Resize((EventHandler)PlotForm_Resize);
			((Control)panelPlot).ResumeLayout(false);
			((ISupportInitialize)picPlot).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)tabPlot).ResumeLayout(false);
			((Control)tabPage1).ResumeLayout(false);
			((Control)panelTab1).ResumeLayout(false);
			((Control)tabPage2).ResumeLayout(false);
			((Control)panelTab6).ResumeLayout(false);
			((ISupportInitialize)picFourier).EndInit();
			((Control)tabPage3).ResumeLayout(false);
			((Control)tabPage3).PerformLayout();
			((Control)panel9).ResumeLayout(false);
			((Control)panel9).PerformLayout();
			((Control)panel10).ResumeLayout(false);
			((Control)panel10).PerformLayout();
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((ISupportInitialize)picComp3).EndInit();
			((ISupportInitialize)picComp2).EndInit();
			((ISupportInitialize)picComp1).EndInit();
			((ISupportInitialize)picTarget).EndInit();
			((Control)tabPage4).ResumeLayout(false);
			((Control)panelTab2).ResumeLayout(false);
			((Control)panelFind).ResumeLayout(false);
			((Control)panelFind).PerformLayout();
			((Control)grpMaxDurn).ResumeLayout(false);
			((Control)grpMaxDurn).PerformLayout();
			((ISupportInitialize)updnMaxDurn).EndInit();
			((Control)grpExpected).ResumeLayout(false);
			((Control)grpExpected).PerformLayout();
			((ISupportInitialize)updnMagStar).EndInit();
			((ISupportInitialize)updnMagObject).EndInit();
			((ISupportInitialize)updnMagDrop).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)tabPage5).ResumeLayout(false);
			((Control)panelTab3).ResumeLayout(false);
			((Control)panelEvents).ResumeLayout(false);
			((Control)panelEvents).PerformLayout();
			((Control)panelExpectedMagDrop).ResumeLayout(false);
			((Control)panelExpectedMagDrop).PerformLayout();
			((ISupportInitialize)updnExpectedMagDrop).EndInit();
			((Control)tabPage6).ResumeLayout(false);
			((Control)panelTab4).ResumeLayout(false);
			((Control)panelDR).ResumeLayout(false);
			((Control)panelDR).PerformLayout();
			((Control)panelUncertainty).ResumeLayout(false);
			((Control)panelUncertainty).PerformLayout();
			((ISupportInitialize)picRUncert).EndInit();
			((ISupportInitialize)picDUncert).EndInit();
			((Control)panelTransitions).ResumeLayout(false);
			((Control)panelTransitions).PerformLayout();
			((ISupportInitialize)picChiR).EndInit();
			((ISupportInitialize)picChiD).EndInit();
			((Control)panelGetLocations).ResumeLayout(false);
			((Control)panelGetLocations).PerformLayout();
			((ISupportInitialize)updnConfidenceR).EndInit();
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((ISupportInitialize)updnConfidenceD).EndInit();
			((Control)panelSN).ResumeLayout(false);
			((Control)panelSN).PerformLayout();
			((Control)panelChi2).ResumeLayout(false);
			((Control)panelChi2).PerformLayout();
			((ISupportInitialize)updnTransitionNumber).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panelMonteCarlo).ResumeLayout(false);
			((Control)panelMonteCarlo).PerformLayout();
			((ISupportInitialize)updnMonteCarlo).EndInit();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((ISupportInitialize)updnSDLimit).EndInit();
			((Control)panelAdjustRegions).ResumeLayout(false);
			((Control)panelAdjustRegions).PerformLayout();
			((ISupportInitialize)updnExtraPoints).EndInit();
			((Control)tabPage7).ResumeLayout(false);
			((Control)panelTab5).ResumeLayout(false);
			((Control)panelTab5).PerformLayout();
			((Control)panelView).ResumeLayout(false);
			((Control)panelView).PerformLayout();
			((Control)grpSaveImages).ResumeLayout(false);
			((Control)grpSaveImages).PerformLayout();
			((Control)panel12).ResumeLayout(false);
			((Control)panel12).PerformLayout();
			((Control)grpFinalResults).ResumeLayout(false);
			((Control)grpFinalResults).PerformLayout();
			((Control)grpCameraCorrections).ResumeLayout(false);
			((Control)grpCameraCorrections).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)updnVerticalScaleAdjustment).EndInit();
			((ISupportInitialize)updnScale).EndInit();
			((Control)panelComp).ResumeLayout(false);
			((Control)panelComp).PerformLayout();
			((Control)pnlScale).ResumeLayout(false);
			((Control)pnlScale).PerformLayout();
			((Control)panel11).ResumeLayout(false);
			((Control)panel11).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
