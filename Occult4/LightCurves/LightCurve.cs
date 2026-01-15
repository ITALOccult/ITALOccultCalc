using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Net;
using System.Net.Mail;
using System.Windows.Forms;
using AOTA;
using Occult;
using Occult.Asteroid_Observations;
using Occult.Lunar_Observations;
using Occult.Properties;

namespace LightCurves
{
	public class LightCurve : Form
	{
		private bool IsLoading;

		private bool ShowToolTip;

		private static bool ShowShortEvent;

		private static bool PreviousShowShort;

		private const int ExtraHeight = 196;

		private bool CC_ListChanged;

		private bool GetToolTip = true;

		internal double OldScaleValue;

		internal LightCurveData LCD;

		internal int RunningAverageCount = 25;

		private ArrayList ExistingFiles = new ArrayList();

		private Timer T = new Timer();

		private Timer Tcopy = new Timer();

		private IContainer components;

		internal NumericUpDown updnScale;

		internal Panel panelPlot;

		internal PictureBox picPlot;

		private GroupBox grpScale;

		private Button cmd_x1;

		internal NumericUpDown updnVerticalScaleAdjustment;

		private Button cmd_x15;

		private Button cmd_x10;

		private Button cmd_x5;

		private Label label1;

		private Label label2;

		internal GroupBox panelComp;

		internal ComboBox cmbPlotAverage;

		internal CheckBox chkComp2;

		internal CheckBox chkTarget;

		internal CheckBox chkShowBackground;

		internal CheckBox chkComp1;

		internal CheckBox chkComp3;

		private Label label3;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openCSVFileToolStripMenuItem;

		private ToolStripMenuItem showFrameTimeInToolTipToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolTip toolTip;

		private ToolStripMenuItem pasteCSVDataSetToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripSeparator toolStripSeparator2;

		private Button cmdIntegrity;

		private Button cmdIntegration;

		private Button cmdSetTimeScale;

		private GroupBox grpRegion;

		private RadioButton optEnd;

		private RadioButton optStart;

		private Label label4;

		private Label label5;

		private Label label7;

		private Label label6;

		private Label label13;

		internal TextBox txtStart;

		internal TextBox txtEnd;

		internal RadioButton optLunar;

		internal RadioButton optAsteroid;

		internal RadioButton optOther;

		private Panel panel1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private Button cmdSaveSend;

		private TextBox txtAddresses;

		private Label lblEmails;

		private Button cmdSaveCCAddresses;

		private Button cmdReLoadCCAddresses;

		private ToolStripMenuItem saveEmailReportToolStripMenuItem;

		private ToolStripMenuItem emailMultipleSavedReportsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private Panel panelEmailAddresses;

		private Button cmdHideEmails;

		private Label label34;

		private ToolStripMenuItem setEmailAddressesToolStripMenuItem;

		private Button cmdEmailMultiple;

		private Button cmdSetEmails;

		private Button cmdSave;

		private PictureBox pictureBox1;

		private ToolStripMenuItem specifyDetailsOfAsteroidalOccultationToolStripMenuItem;

		private Button cmdSpecifyDetails;

		internal TextBox txtNumDataPoints;

		private Label label37;

		private ToolStripSeparator toolStripSeparator4;

		private Button cmdExit_CC;

		private TextBox textStar;

		private TextBox txtObs;

		private TextBox txtCirc;

		private Label label41;

		private Label label42;

		private Label label43;

		private Panel panelControls;

		internal Label lblFileName;

		private Label label8;

		internal TextBox txtFramesIntegrated;

		private Label label9;

		private Button cmdGmailHelp;

		private Panel panel2;

		internal Panel pnlShort;

		private Panel panel9;

		private Panel panel10;

		internal Label lblPossible;

		internal Label lblProbable;

		internal Label label74;

		internal Label label73;

		internal Label lblRatio12;

		private Label label72;

		internal PictureBox picComp3;

		internal PictureBox picComp2;

		internal PictureBox picComp1;

		internal PictureBox picTarget;

		private Panel panel8;

		internal Label label47;

		private Panel panel7;

		internal RadioButton opt9;

		internal RadioButton opt57;

		internal RadioButton opt41;

		internal RadioButton opt49;

		internal RadioButton opt33;

		internal RadioButton opt25;

		internal RadioButton opt17;

		internal Label lblNoCompStars;

		private Label lblTarget;

		internal Label lblComp1;

		internal Label lblComp2;

		internal Label lblComp3;

		private Label label69;

		internal Label lblHead2;

		internal Label lblHead3;

		internal Label lblHead1;

		internal ListBox lstComp3;

		internal ListBox lstComp2;

		internal ListBox lstComp1;

		internal ListBox lstTarget;

		private ToolStripMenuItem hideToolStripMenuItem;

		private Label label75;

		private Label label71;

		private Label label70;

		private Label label68;

		private Label label67;

		private Button cmdHelp;

		private ToolStripMenuItem showShortEventEvaluationToolStripMenuItem;

		private ToolStripMenuItem show_ShortEventEvaluationToolStripMenuItem;

		private Label lblShortEventWarning;

		private Label label10;

		internal Label lblFrame;

		private Label lblCopied;

		internal RadioButton optComp1;

		internal RadioButton optComp2;

		internal RadioButton optComp3;

		private ToolStripMenuItem saveImageOfThisFormToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem1;

		private ToolStripMenuItem setScaleToMatchMonitor100ToolStripMenuItem;

		private ToolStripMenuItem rightclickToSetFullLightLevelToolStripMenuItem;

		internal CheckBox chkScaleComp;

		private ToolStripMenuItem alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		internal ToolStripMenuItem correctForMissingExposuresToolStripMenuItem;

		private Button cmdManageReportFiles;

		public LightCurve()
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Expected O, but got Unknown
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_0030: Expected O, but got Unknown
			InitializeComponent();
			((Control)this).set_Height(550);
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
			PreviousShowShort = ShowShortEvent;
			if (ShowShortEvent)
			{
				((Control)this).set_Height(746);
				((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).set_Text("Hide Short event evaluation");
				T.Start();
			}
		}

		private void LightCurve_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			OldScaleValue = (double)updnScale.get_Value();
			((ListControl)cmbPlotAverage).set_SelectedIndex(0);
			SetPanelVisibility();
			SetSizes(ResizeEvent: true);
			((ToolStripItem)setScaleToMatchMonitor100ToolStripMenuItem).set_Text("Set scale (currently " + Settings.Default.MonitorScale + "%) to match Monitor scale");
			PreviousShowShort = (ShowShortEvent = Settings.Default.ShowShortEvent);
			alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem.set_Checked(Settings.Default.AOTAshowComparisons);
			if (ShowShortEvent)
			{
				((Control)this).set_Height(746);
				((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).set_Text("Hide Short event evaluation");
				T.Start();
			}
		}

		private void LightCurve_Resize(object sender, EventArgs e)
		{
			SetSizes(ResizeEvent: true);
		}

		internal void SetSizes(bool ResizeEvent)
		{
			try
			{
				if (PreviousShowShort != ShowShortEvent)
				{
					PreviousShowShort = ShowShortEvent;
					if (ShowShortEvent)
					{
						((Control)this).set_Height(((Control)this).get_Height() + 196);
					}
					else
					{
						((Control)this).set_Height(((Control)this).get_Height() - 196);
					}
				}
				if (ShowShortEvent & (((Control)this).get_Height() < 540))
				{
					((Control)this).set_Height(540);
				}
				if (!ShowShortEvent & (((Control)this).get_Height() < 340))
				{
					((Control)this).set_Height(340);
				}
				if (((Control)this).get_Width() < 1020)
				{
					((Control)this).set_Width(1020);
				}
				int num = 200;
				num = (int)((decimal)AOTAData.StarCountToPlot * updnScale.get_Value());
				if (num < 200)
				{
					num = 200;
				}
				if (num > 32000)
				{
					num = 32000;
				}
				((Control)panelPlot).set_Top(25);
				((Control)panelPlot).set_Width(((Control)this).get_Width() - 40);
				if (ShowShortEvent)
				{
					((Control)panelPlot).set_Height(((Control)this).get_Height() - 287 - 196);
				}
				else
				{
					((Control)panelPlot).set_Height(((Control)this).get_Height() - 287);
				}
				((Control)panelControls).set_Top(((Control)panelPlot).get_Bottom());
				((Control)pnlShort).set_Top(((Control)panelControls).get_Bottom() + 5);
				((Control)picPlot).set_Width(num);
				((Control)picPlot).set_Height(((Control)panelPlot).get_Height() - 20);
				((Control)panelEmailAddresses).set_Top(((Control)cmdSetEmails).get_Bottom() - ((Control)panelEmailAddresses).get_Height() + 2);
				OldScaleValue = (double)updnScale.get_Value();
				PreviousShowShort = ShowShortEvent;
				if (ResizeEvent)
				{
					AOTAData.PlotLightCurve();
				}
			}
			catch
			{
			}
		}

		internal void SetPanelVisibility()
		{
			((Control)cmdSpecifyDetails).set_Visible(optAsteroid.get_Checked());
		}

		internal void SetAsLunar()
		{
			((Control)optLunar).set_Enabled(true);
			optLunar.set_Checked(true);
			RadioButton obj = optAsteroid;
			RadioButton obj2 = optOther;
			bool flag;
			((ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem).set_Enabled(flag = false);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag);
			((Control)obj).set_Enabled(enabled);
		}

		public void SetAsAsteroidal()
		{
			RadioButton obj = optAsteroid;
			bool enabled;
			((ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem).set_Enabled(enabled = true);
			((Control)obj).set_Enabled(enabled);
			optAsteroid.set_Checked(true);
			RadioButton obj2 = optLunar;
			((Control)optOther).set_Enabled(enabled = false);
			((Control)obj2).set_Enabled(enabled);
			LightData.LightCurveForm.LCD = new LightCurveData();
			AOTAData.StarCount = (AOTAData.StarCountToPlot = 0);
			AOTAData.PlotLightCurve();
		}

		internal void SetAsOther()
		{
			((Control)optOther).set_Enabled(true);
			optOther.set_Checked(true);
			RadioButton obj = optLunar;
			RadioButton obj2 = optAsteroid;
			bool flag;
			((ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem).set_Enabled(flag = false);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag);
			((Control)obj).set_Enabled(enabled);
		}

		internal void Set_AllowAll()
		{
			RadioButton obj = optOther;
			RadioButton obj2 = optLunar;
			bool flag;
			((Control)optAsteroid).set_Enabled(flag = true);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag);
			((Control)obj).set_Enabled(enabled);
			optOther.set_Checked(true);
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

		private void updnScale_ValueChanged(object sender, EventArgs e)
		{
			SetSizes(ResizeEvent: false);
			AOTAData.PlotLightCurve();
		}

		private void updnVerticalScaleAdjustment_ValueChanged(object sender, EventArgs e)
		{
			AOTAData.VerticalScaleAdjustment = (float)updnVerticalScaleAdjustment.get_Value();
			AOTAData.PlotLightCurve();
		}

		private void openCSVFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			OpenNewFile("", -1, optLunar.get_Checked());
		}

		private void pasteCSVDataSetToolStripMenuItem_Click(object sender, EventArgs e)
		{
			RadioButton obj = optComp1;
			RadioButton obj2 = optComp2;
			bool flag;
			optComp3.set_Checked(flag = false);
			bool @checked;
			obj2.set_Checked(@checked = flag);
			obj.set_Checked(@checked);
			AOTAData.PasteCSVdata(FromLightCurve: true, correctForMissingExposuresToolStripMenuItem.get_Checked());
			SetSomeParameters();
		}

		internal void OpenNewFile(string eVscopeID, int EventDetailsIndex, bool IsLunar)
		{
			IsLoading = true;
			RadioButton obj = optComp1;
			RadioButton obj2 = optComp2;
			bool flag;
			optComp3.set_Checked(flag = false);
			bool @checked;
			obj2.set_Checked(@checked = flag);
			obj.set_Checked(@checked);
			if (!optLunar.get_Checked())
			{
				LCD = new LightCurveData();
			}
			AOTAData.OpenCSVfile(FromLightCurve: true, eVscopeID, correctForMissingExposuresToolStripMenuItem.get_Checked());
			if (eVscopeID.Length > 0)
			{
				optAsteroid.set_Checked(true);
			}
			if (EventDetails.Observers.Count > 0 && EventDetailsIndex >= 0)
			{
				LCD.AsteroidName = EventDetails.AsteroidID;
				LCD.AsteroidNumber = EventDetails.AsteroidNo;
				LCD.Year = EventDetails.Year;
				LCD.Month = EventDetails.Month;
				LCD.Day = EventDetails.Day;
				LCD.Observer = EventDetails.Observers[EventDetailsIndex].Observer1;
				if (eVscopeID.Length > 0)
				{
					LCD.Observer += "/Unistellar";
				}
				LCD.SetLongitude_fromDeg = EventDetails.Observers[EventDetailsIndex].Longitude;
				LCD.SetLatitude_fromDeg = EventDetails.Observers[EventDetailsIndex].Latitude;
				LCD.AltM = EventDetails.Observers[EventDetailsIndex].Altitude;
				LightData.LightCurveForm.LCD.NoStarNumberExpected = false;
				if (EventDetails.StarCat == "HIP")
				{
					LightData.LightCurveForm.LCD.Hipparcos = int.Parse(EventDetails.StarNumber);
				}
				else if (EventDetails.StarCat == "Tycho2")
				{
					string[] array = EventDetails.StarNumber.Split(new char[1] { '-' });
					LightData.LightCurveForm.LCD.Tyc2A = short.Parse(array[0]);
					LightData.LightCurveForm.LCD.Tyc2B = short.Parse(array[1]);
					LightData.LightCurveForm.LCD.Tyc2C = short.Parse(array[2]);
				}
				else if (EventDetails.StarCat == "UCAC4")
				{
					string[] array2 = EventDetails.StarNumber.Split(new char[1] { '-' });
					LightData.LightCurveForm.LCD.U4Zone = int.Parse(array2[0]);
					LightData.LightCurveForm.LCD.U4Number = int.Parse(array2[1]);
				}
				else
				{
					LightData.LightCurveForm.LCD.NoStarNumberExpected = true;
				}
			}
			else if (!IsLunar)
			{
				LCD.AsteroidName = "";
				LCD.AsteroidNumber = -1;
				LCD.Year = DateTime.Now.Year;
				LCD.Month = DateTime.Now.Month;
				LCD.Day = DateTime.Now.Day;
				LightData.LightCurveForm.LCD.Hipparcos = 0;
				LightData.LightCurveForm.LCD.Tyc2B = 0;
				LightData.LightCurveForm.LCD.SAO = 0;
				LightData.LightCurveForm.LCD.XZ = 0;
				LightData.LightCurveForm.LCD.U4Number = 0;
				LightData.LightCurveForm.LCD.NoStarNumberExpected = false;
				LCD.Observer = "";
				LightCurveData lCD = LCD;
				LightCurveData lCD2 = LCD;
				int num2 = (LCD.AltM = 0);
				double num5 = (lCD.SetLongitude_fromDeg = (lCD2.SetLatitude_fromDeg = num2));
			}
			IsLoading = false;
			SetSomeParameters();
		}

		private void SetSomeParameters()
		{
			((Form)LightData.LightCurveForm).set_WindowState((FormWindowState)0);
			AOTAData.StartOfLightCurveSelection = 0;
			AOTAData.EndOfLightCurveSelection = AOTAData.StarCount - 1;
			if (AOTAData.EndOfLightCurveSelection > 0)
			{
				LCD.Duration = AOTAData.FrameTimeSecs[AOTAData.EndOfLightCurveSelection] - AOTAData.FrameTimeSecs[AOTAData.StartOfLightCurveSelection];
			}
			SetStartTime(0);
			optStart.set_Checked(true);
			((Control)txtNumDataPoints).set_Text(string.Format("{0,1:f0}", (AOTAData.EndOfLightCurveSelection - AOTAData.StartOfLightCurveSelection) / AOTAData.NumberOfFramesBinned + 1));
			((Control)txtFramesIntegrated).set_Text(AOTAData.NumberOfFramesBinned.ToString());
			SetSizes(ResizeEvent: false);
			SetTextBoxes();
			GetPotentialDuplicateFiles();
			AOTAData.PlotLightCurve();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Report Light Curve");
		}

		private void showFrameTimeInToolTipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowToolTip = !ShowToolTip;
			showFrameTimeInToolTipToolStripMenuItem.set_Checked(ShowToolTip);
			toolTip.set_Active(ShowToolTip);
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveReport();
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			SaveReport();
		}

		private void SaveReport()
		{
			//IL_0067: Unknown result type (might be due to invalid IL or missing references)
			if (ValidDataSet("save"))
			{
				using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Observations\\LightCurves\\" + LightData.LightCurveForm.LCD.FileNameForSave))
				{
					streamWriter.Write(LCD.ToString());
				}
				MessageBox.Show("Light curve saved as " + LightData.LightCurveForm.LCD.FileNameForSave);
			}
			AOTAData.LightCurveReportHasBeenSaved = true;
		}

		private void saveToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_00c6: Unknown result type (might be due to invalid IL or missing references)
			double num = (double)Settings.Default.MonitorScale / 100.0;
			int width = (int)(num * (double)((Control)this).get_Bounds().Width);
			int height = (int)(num * (double)((Control)this).get_Bounds().Height);
			Bitmap bitmap = new Bitmap(width, height);
			Graphics.FromImage(bitmap).CopyFromScreen((int)(num * (double)((Form)this).get_Location().X), (int)(num * (double)((Form)this).get_Location().Y), 0, 0, bitmap.Size);
			try
			{
				string text = Settings.Default.LightCurveReporterImage_Dir + "\\" + Path.GetFileNameWithoutExtension(LightData.LightCurveForm.LCD.FileNameForSave);
				Settings.Default.LightCurveReporterImage_Dir = Output.SaveGraphic(bitmap, text, Path.GetDirectoryName(text));
			}
			catch
			{
				MessageBox.Show("Error - file not saved");
			}
		}

		private void setScaleToMatchMonitor100ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new MonitorScale()).ShowDialog();
			((ToolStripItem)setScaleToMatchMonitor100ToolStripMenuItem).set_Text("Set scale (currently " + Settings.Default.MonitorScale + "%) to match Monitor scale");
		}

		private bool ValidDataSet(string SaveSend)
		{
			//IL_02cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d5: Invalid comparison between Unknown and I4
			string text = "";
			if (LCD.NumPoints == 0)
			{
				text += "* No light curve data has been selected\r\n\r\n";
			}
			if (AOTAData.EndOfLightCurveSelection < AOTAData.StartOfLightCurveSelection)
			{
				text += "* The end of the selected light curve occurs before the start\r\n\r\n";
			}
			if (LCD.Duration == 0.0)
			{
				text += "* The duration of the light curve has not been set. You probably need to manually set the time scale using 'Set time scale'.\r\n\r\n";
			}
			if (LCD.Duration < 0.0)
			{
				text += string.Format("* The duration of the light curve [{0,1:f2}secs] is invalid as it is negative. Please check for errors (including OCR errors) in the time base.\r\n\r\n", LCD.Duration);
			}
			if ((LCD.Duration > 120.0) & (LCD.MoonSize < 2.0))
			{
				text += string.Format("* The duration of the light curve is too large for a lunar occultation, being {0,1:f2}secs. Either there is an error with the time base, or the light curve should be split into shorter time segments.\r\n\r\n", LCD.Duration);
			}
			if ((LCD.Duration > 1800.0) & (LCD.MoonSize > 2.0))
			{
				text += string.Format("* The duration of the light curve appears to be too large for an asteroidal occultation, being {0,1:f2}secs. Please check for errors (including OCR errors) in the time base.\r\n\r\n", LCD.Duration);
			}
			if (LCD.MoonSize > 2.0)
			{
				if (LCD.Observer.Length < 2)
				{
					text += "* You havent entered the observer's name. Make sure you have used the 'Set star event and Observer details buttom\r\n\r\n";
				}
				if (LCD.EventJD < 2444239.0)
				{
					text += "* You haven't entered the date of the event. Make sure you have used the 'Set star event and Observer details buttom\r\n\r\n";
				}
				if (LCD.ObserverLongitude == 0.0)
				{
					text += "* You haven't entered the observer's longitude. Make sure you have used the 'Set star event and Observer details buttom\r\n\r\n";
				}
				if (LCD.ObserverLatitude == 0.0)
				{
					text += "* You haven't entered the observer's latitude. Make sure you have used the 'Set star event and Observer details buttom\r\n\r\n";
				}
				if (LCD.AsteroidNumber < 1)
				{
					text += "* You haven't entered the asteroid's number. Make sure you have used the 'Set star event and Observer details buttom\r\n\r\n";
				}
				if ((((LCD.Tyc2A == 0) | (LCD.Tyc2B == 0)) & (LCD.Hipparcos == 0) & ((LCD.U4Zone == 0) | (LCD.U4Number == 0)) & ((LCD.B1Zone == 0) | (LCD.B1Number == 0)) & ((LCD.N1Zone == 0) | (LCD.N1Number == 0))) && !LCD.NoStarNumberExpected)
				{
					text += "* You haven't entered the star number. Make sure you have used the 'Set star event and Observer details' button\r\nFor star numbers like 'G101204.3-124652' no star entry should be made\r\n\r\n";
				}
			}
			if (text.Length > 0)
			{
				if ((int)MessageBox.Show("The following possible errors have been detected in your data\r\n\r\n" + text + "You should fix these errors before continuing. \r\n\r\nDo you want to continue to " + SaveSend + " the data, despite these errors?", "Error in light curve data", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
				{
					return true;
				}
				return false;
			}
			return true;
		}

		private void cmbPlotAverage_SelectedIndexChanged(object sender, EventArgs e)
		{
			AOTAData.PlotRunningAverageLC = int.Parse(cmbPlotAverage.get_Items().get_Item(((ListControl)cmbPlotAverage).get_SelectedIndex()).ToString());
			AOTAData.PlotLightCurve();
		}

		private void chkTarget_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotLightCurve();
			}
		}

		private void chkComp1_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotLightCurve();
			}
		}

		private void chkComp2_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotLightCurve();
			}
		}

		private void chkComp3_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotLightCurve();
			}
		}

		private void chkScaleComp_CheckedChanged(object sender, EventArgs e)
		{
			AOTAData.PlotLightCurve();
		}

		private void chkShowBackground_CheckedChanged(object sender, EventArgs e)
		{
			if (!IsLoading)
			{
				AOTAData.PlotLightCurve();
			}
		}

		private void updnVerticalScaleAdjustment_ValueChanged_1(object sender, EventArgs e)
		{
			AOTAData.VerticalScaleAdjustmentLC = (float)updnVerticalScaleAdjustment.get_Value();
			AOTAData.PlotLightCurve();
		}

		private void picPlot_MouseClick(object sender, MouseEventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_001c: Invalid comparison between Unknown and I4
			if (AOTAData.StarCount < 1 || AOTAData.IsPlotting)
			{
				return;
			}
			if ((int)e.get_Button() == 2097152)
			{
				float num = e.get_Y();
				if (num < 5f)
				{
					AOTAData.LightCurve_Height_100 = 0f;
				}
				else
				{
					AOTAData.LightCurve_Height_100 = num / (float)((Control)picPlot).get_Height();
				}
				AOTAData.PlotLightCurve();
				return;
			}
			int num2 = (int)((double)(float)e.get_X() / OldScaleValue + 0.5);
			int num3 = AOTAData.FirstFrameUsedInAnalysis + AOTAData.NumberOfFramesBinned * num2;
			if (num3 < 0)
			{
				num3 = 0;
			}
			if (num3 >= AOTAData.StarCount)
			{
				num3 = AOTAData.StarCount - 1;
			}
			if (optStart.get_Checked())
			{
				AOTAData.StartOfLightCurveSelection = AOTAData.NumberOfFramesBinned * num2;
				((Control)txtStart).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[num2]));
				optEnd.set_Checked(true);
				SetStartTime(num3);
			}
			else if (optEnd.get_Checked())
			{
				AOTAData.EndOfLightCurveSelection = AOTAData.NumberOfFramesBinned * num2;
				((Control)txtEnd).set_Text(string.Format("{0,1:f1}", AOTAData.FrameIDofPlot[num2]));
				optStart.set_Checked(true);
			}
			LCD.Duration = AOTAData.FrameTimeSecs[AOTAData.EndOfLightCurveSelection] - AOTAData.FrameTimeSecs[AOTAData.StartOfLightCurveSelection];
			if (LCD.Duration < -10000.0)
			{
				LCD.Duration += 86400.0;
			}
			LCD.NumPoints = (AOTAData.EndOfLightCurveSelection - AOTAData.StartOfLightCurveSelection) / AOTAData.NumberOfFramesBinned + 1;
			((Control)txtNumDataPoints).set_Text(string.Format("{0,1:f0}", LCD.NumPoints));
			((Control)txtFramesIntegrated).set_Text(AOTAData.NumberOfFramesBinned.ToString());
			if (LCD.NumPoints > 1)
			{
				LCD.Interval = LCD.Duration / (double)(LCD.NumPoints - 1);
			}
			else
			{
				LCD.Interval = 0.0;
			}
			AOTAData.PlotLightCurve();
			((Control)lblFileName).set_Text(LCD.FileNameForSave);
			GetPotentialDuplicateFiles();
		}

		private void SetStartTime(int FrameNo)
		{
			LCD.SetHMS = Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[FrameNo] / 3600.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: true);
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
				float num5 = (float)((Control)picPlot).get_Height() - AOTAData.LightCurve_ZeroHeight;
				float num6 = 100f * (num5 - num2) / (num5 - AOTAData.LightCurve_Height_100 * (float)((Control)picPlot).get_Height());
				string text = string.Format(" | Height: {0,1:f0}", num6);
				if (AOTAData.LightCurve_Height_100 > 0f)
				{
					text += "%";
				}
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip.SetToolTip((Control)(object)picPlot, AOTAData.FrameID[num4] + " | " + Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[num4] / 3600.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: true) + text);
				}
			}
		}

		private void cmdIntegrity_Click(object sender, EventArgs e)
		{
			AOTAData.ShowIntegrityCheck(FromLightCurve: true);
			AOTAData.PlotLightCurve();
		}

		private void cmdSetTimeScale_Click(object sender, EventArgs e)
		{
			AOTAData.SetTimeScale();
			SetStartTime(0);
			AOTAData.PlotLightCurve();
		}

		private void cmdIntegration_Click(object sender, EventArgs e)
		{
			AOTAData.Set_DataToPlot(FromLightCurve: true);
			AOTAData.PlotLightCurve();
		}

		private void optLunar_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optAsteroid_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optOther_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		internal void SetTextBoxes()
		{
			if (LCD != null)
			{
				((Control)textStar).set_Text(LCD.StarDetails());
				((Control)txtObs).set_Text(LCD.DateDetails() + "\r\n" + LCD.ObserverDetails());
				((Control)txtCirc).set_Text(LCD.Circumstances());
				((Control)lblFileName).set_Text(LCD.FileNameForSave);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			if (LCD.LightValues == null)
			{
				text = "No light curve data";
			}
			else if (LCD.LightValues.Count < 1)
			{
				text = "No light curve data";
			}
			Clipboard.SetText(LCD.ToString() + text);
		}

		private void cmdSaveSend_Click(object sender, EventArgs e)
		{
			SaveSend();
		}

		private void SaveSend()
		{
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0105: Invalid comparison between Unknown and I4
			//IL_02b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Unknown result type (might be due to invalid IL or missing references)
			if (!ValidDataSet("save & email"))
			{
				return;
			}
			string text = Utilities.AppPath + "\\Observations\\LightCurves\\";
			string text2 = Utilities.AppPath + "\\Observations\\LightCurves\\Reported\\";
			if (LCD.LightValues.Count < 1)
			{
				MessageBox.Show("No light curve data to save & email");
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet.\r\n\r\nIf the internet cannot be connected, save the report using the 'Save report' button.", "No internet");
				return;
			}
			string lightCurve_EmailAddress = http.Get_LightCurve_EmailAddress();
			if (lightCurve_EmailAddress.Length < 4)
			{
				MessageBox.Show("No valid address to email the Light Curve.\r\n\r\nPlease go to the Downloads form and download the latest 'Reporting addresses for occultations'");
				return;
			}
			using (StreamWriter streamWriter = new StreamWriter(text + LightData.LightCurveForm.LCD.FileNameForSave))
			{
				streamWriter.Write(LCD.ToString());
			}
			string text3 = "";
			string subject = "Light curve report";
			string body = "Light curve report attached\r\n\r\n";
			string to = lightCurve_EmailAddress;
			if (((Settings.Default.FTP_AnonymousPassword.Length < 5) | (Settings.Default.EMailServerName.Length < 5)) && (int)((Form)new SetEmailAndServer()).ShowDialog() == 2)
			{
				return;
			}
			string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
			string fTP_AnonymousPassword2 = Settings.Default.FTP_AnonymousPassword;
			string eMailServerName = Settings.Default.EMailServerName;
			MailMessage mailMessage = new MailMessage(fTP_AnonymousPassword2, to, subject, body);
			mailMessage.Bcc.Add(fTP_AnonymousPassword);
			string[] array = Settings.Default.LightCurve_CC_Addresses.Split(new char[1] { ';' });
			for (int i = 0; i < array.GetUpperBound(0); i++)
			{
				if (array[i].Contains("@"))
				{
					mailMessage.CC.Add(array[i].Trim());
				}
			}
			Attachment item = new Attachment(text + LightData.LightCurveForm.LCD.FileNameForSave);
			mailMessage.Attachments.Add(item);
			SmtpClient smtpClient = new SmtpClient(eMailServerName);
			if (Settings.Default.Email.Length > 0)
			{
				smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
			}
			smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
			if (!int.TryParse(Settings.Default.EmailPort, out var result))
			{
				result = -1;
			}
			if (result >= 0)
			{
				smtpClient.Port = result;
			}
			try
			{
				smtpClient.Send(mailMessage);
			}
			catch (SmtpFailedRecipientsException ex)
			{
				for (int j = 0; j < ex.InnerExceptions.Length; j++)
				{
					text3 = text3 + "Failed to deliver message to " + ex.FailedRecipient![j] + "\r\n";
				}
			}
			catch (SmtpException ex2)
			{
				text3 = text3 + ex2.Message + "\r\n\r\nThe light curve has been saved, but not emailed";
			}
			mailMessage.Dispose();
			if (text3.Length > 5)
			{
				MessageBox.Show(text3, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			File.Move(text + LightData.LightCurveForm.LCD.FileNameForSave, text2 + LightData.LightCurveForm.LCD.FileNameForSave);
			MessageBox.Show("The light curve has been emailed, and the file moved to the '\\Observations\\LightCurves\\Reported' folder", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void cmdSaveCCAddresses_Click(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("The Save will overwrite the following addresses:\r\n\r\n" + Settings.Default.LightCurve_CC_Addresses + "\r\n\r\nDo you want to continue?", "Verify replace addresses", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				if (!((Control)txtAddresses).get_Text().EndsWith(";"))
				{
					((Control)txtAddresses).set_Text(((Control)txtAddresses).get_Text() + ";");
				}
				Settings.Default.LightCurve_CC_Addresses = ((Control)txtAddresses).get_Text();
				CC_ListChanged = false;
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip.SetToolTip((Control)(object)cmdReLoadCCAddresses, Settings.Default.LightCurve_CC_Addresses);
				}
			}
		}

		private void cmdReLoadCCAddresses_Click(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("All edits to the displayed list will be lost, and replaced with\r\n\r\n" + Settings.Default.LightCurve_CC_Addresses + "\r\n\r\n Do you want to continue?", "Verify restore addresses", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Put_CC_AddressesInEditBox();
			}
		}

		private void Put_CC_AddressesInEditBox()
		{
			((Control)txtAddresses).set_Text(Settings.Default.LightCurve_CC_Addresses);
			if (!((Control)txtAddresses).get_Text().EndsWith(";"))
			{
				((Control)txtAddresses).set_Text(((Control)txtAddresses).get_Text() + ";");
			}
			CC_ListChanged = false;
		}

		private void emailMultipleSavedReportsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			http.Email_LightCurveReports();
		}

		private void setEmailAddressesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)panelEmailAddresses).set_Visible(true);
		}

		private void cmdEmailMultiple_Click(object sender, EventArgs e)
		{
			http.Email_LightCurveReports();
		}

		private void cmdSetEmails_Click(object sender, EventArgs e)
		{
			((Control)panelEmailAddresses).set_Visible(true);
			Put_CC_AddressesInEditBox();
		}

		private void specifyDetailsOfAsteroidalOccultationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0031: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Invalid comparison between Unknown and I4
			if (optAsteroid.get_Checked() && (LCD.NumPoints != 0 || (int)MessageBox.Show("Light curve data has not been entered yet.\r\n\r\nIf you enter the light curve data using the File... Open\r\n menu item, the event data will be reset.\r\n\r\nThis is not the case if you Paste the CSV data.\r\n\r\nDo you wish to continue?", "Confirm data loss with OPEN", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7))
			{
				LightData.ShowAsteroidLCdataInput();
			}
		}

		private void cmdSpecifyDetails_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			//IL_002a: Invalid comparison between Unknown and I4
			if (LCD.NumPoints != 0 || (int)MessageBox.Show("Light curve data has not been entered yet.\r\n\r\nIf you enter the light curve data using the File... Open\r\n menu item, the event data will be reset.\r\n\r\nThis is not the case if you Paste the CSV data.\r\n\r\nDo you wish to continue?", "Confirm data loss with OPEN", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				LightData.ShowAsteroidLCdataInput();
				SetTextBoxes();
				GetPotentialDuplicateFiles();
			}
		}

		private void cmdExit_CC_Click(object sender, EventArgs e)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Invalid comparison between Unknown and I4
			if (!CC_ListChanged || (int)MessageBox.Show("You have made changes to the CC addresses that have not been saved.\r\n\r\nDo you want to continue with Exit?", "Unsaved changes to CC addresses", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((Control)panelEmailAddresses).set_Visible(false);
			}
		}

		private void cmdHideEmails_Click(object sender, EventArgs e)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Invalid comparison between Unknown and I4
			if (!CC_ListChanged || (int)MessageBox.Show("You have made changes to the CC addresses that have not been saved.\r\n\r\nDo you want to continue with Exit?", "Unsaved changes to CC addresses", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((Control)panelEmailAddresses).set_Visible(false);
			}
		}

		private void txtAddresses_TextChanged(object sender, EventArgs e)
		{
			CC_ListChanged = true;
		}

		private void SaveSendEmailReportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveSend();
		}

		internal void GetPotentialDuplicateFiles()
		{
			int num = -1;
			int num2 = -1;
			int result = 0;
			int result2 = 0;
			int result3 = 0;
			int result4 = 0;
			int result5 = 0;
			int result6 = 0;
			int num3 = 20;
			if (LightData.LightCurveForm.LCD.IsAsteroid)
			{
				num3 = 600;
			}
			ExistingFiles.Clear();
			string[] array = new string[2]
			{
				Utilities.AppPath + "\\Observations\\LightCurves",
				Utilities.AppPath + "\\Observations\\LightCurves\\Reported"
			};
			string[] array2 = new string[2] { ", in \\Observations\\LightCurves", ", in \\Observations\\LightCurves\\Reported" };
			string starForFileName = LightData.LightCurveForm.LCD.StarForFileName;
			if (starForFileName == "")
			{
				return;
			}
			DateTime dateTime = new DateTime(LightData.LightCurveForm.LCD.Year, LightData.LightCurveForm.LCD.Month, LightData.LightCurveForm.LCD.Day, LightData.LightCurveForm.LCD.Hr, LightData.LightCurveForm.LCD.Min, (int)LightData.LightCurveForm.LCD.Sec);
			for (int i = 0; i < 2; i++)
			{
				string[] files = Directory.GetFiles(array[i], "*.dat");
				for (int j = 0; j < files.Length; j++)
				{
					string fileNameWithoutExtension = Path.GetFileNameWithoutExtension(files[j]);
					num = fileNameWithoutExtension.IndexOf("_");
					if (num >= 0)
					{
						num2 = fileNameWithoutExtension.IndexOf("_", num + 1);
						if (num2 - num == 9 && fileNameWithoutExtension.IndexOf("-", num2 + 1) - num2 == 7 && !(starForFileName != fileNameWithoutExtension.Substring(0, num)) && int.TryParse(fileNameWithoutExtension.Substring(num + 1, 4), out result) && int.TryParse(fileNameWithoutExtension.Substring(num + 5, 2), out result2) && int.TryParse(fileNameWithoutExtension.Substring(num + 7, 2), out result3) && int.TryParse(fileNameWithoutExtension.Substring(num2 + 1, 2), out result4) && int.TryParse(fileNameWithoutExtension.Substring(num2 + 3, 2), out result5) && int.TryParse(fileNameWithoutExtension.Substring(num2 + 5, 2), out result6) && !(Math.Abs((new DateTime(result, result2, result3, result4, result5, result6) - dateTime).TotalSeconds) > (double)num3))
						{
							ExistingFiles.Add(fileNameWithoutExtension + array2[i]);
						}
					}
				}
			}
			if (ExistingFiles.Count > 0)
			{
				LightData.ShowPossibleDuplicates();
				LightData.PossibleDuplicates.lstDups.get_Items().Clear();
				for (int k = 0; k < ExistingFiles.Count; k++)
				{
					LightData.PossibleDuplicates.lstDups.get_Items().Add((object)ExistingFiles[k]!.ToString());
				}
			}
			else if (LightData.PossibleDuplicates != null)
			{
				((Form)LightData.PossibleDuplicates).Close();
			}
		}

		private void LightCurve_FormClosing(object sender, FormClosingEventArgs e)
		{
			Settings.Default.ShowShortEvent = ShowShortEvent;
		}

		private void LightCurve_FormClosed(object sender, FormClosedEventArgs e)
		{
			if (LightData.PossibleDuplicates != null)
			{
				((Form)LightData.PossibleDuplicates).Close();
			}
		}

		private void cmdGmailHelp_Click(object sender, EventArgs e)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("\r\n============== GMail users================\r\nIf you are using gmail for your emails, uploading via Occult\r\nwill probably not work. Send a 'normal' email to\r\n\t 'heraldDR@bigpond.com' \r\nwith the subject line set exactly to \t'Light curve report'\r\nYou will find the light curve files to attach in\r\n\tOccult 4/Observations/LightCurves/\r\n\r\nTo avoid duplicate submissions:\r\nAfter emailing the light curves, move them to a subdirectory\r\nof this directory, called \r\n\tOccult 4/Observations/LightCurves/Reported\r\nIf necessary, create it.\r\n======================================", "Gmail help", (MessageBoxButtons)0, (MessageBoxIcon)64);
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
			AOTAData.ShortEventsCheck(9, ForLightCurve: true);
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
			AOTAData.ShortEventsCheck(17, ForLightCurve: true);
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
			AOTAData.ShortEventsCheck(25, ForLightCurve: true);
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
			AOTAData.ShortEventsCheck(33, ForLightCurve: true);
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
			AOTAData.ShortEventsCheck(41, ForLightCurve: true);
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
			AOTAData.ShortEventsCheck(49, ForLightCurve: true);
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
			RunningAverageCount = 57;
			AOTAData.ShortEventsCheck(57, ForLightCurve: true);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA Short");
		}

		private void show_ShortEventEvaluationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowShortEvent = !ShowShortEvent;
			if (ShowShortEvent)
			{
				((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).set_Text("Hide short event evaluation");
				((Control)lblShortEventWarning).Show();
				T.Start();
			}
			else
			{
				((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).set_Text("Show short event evaluation");
			}
			SetSizes(ResizeEvent: false);
		}

		private void updnBottom_ValueChanged(object sender, EventArgs e)
		{
			AOTAData.PlotLightCurve();
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
			AOTAData.ShortEventsCheck(RunningAverageCount, ForLightCurve: true);
		}

		private void optComp2_Click(object sender, EventArgs e)
		{
			optComp2.set_Checked(!optComp2.get_Checked());
			AOTAData.ShortEventsCheck(RunningAverageCount, ForLightCurve: true);
		}

		private void optComp3_Click(object sender, EventArgs e)
		{
			optComp3.set_Checked(!optComp3.get_Checked());
			AOTAData.ShortEventsCheck(RunningAverageCount, ForLightCurve: true);
		}

		private void alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			//IL_002a: Invalid comparison between Unknown and I4
			if (!alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem.get_Checked() || (int)MessageBox.Show("By clearing this setting, Comparison star light curves will\r\nnot be displayed by default. This will increase the risk\r\nof failing to recognise that an 'event' might only be noise.\r\r\n\r\nDo you want to clear this check", "Confirm clearoing comparison check", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem.set_Checked(!alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem.get_Checked());
				Settings.Default.AOTAshowComparisons = alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem.get_Checked();
			}
		}

		private void correctForMissingExposuresToolStripMenuItem_Click(object sender, EventArgs e)
		{
			correctForMissingExposuresToolStripMenuItem.set_Checked(!correctForMissingExposuresToolStripMenuItem.get_Checked());
		}

		private void cmdManageReportFiles_Click(object sender, EventArgs e)
		{
			Utilities.UploadLightCurvesIsDue(AutoShow: false);
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
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_04a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04af: Expected O, but got Unknown
			//IL_04b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ba: Expected O, but got Unknown
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c5: Expected O, but got Unknown
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d0: Expected O, but got Unknown
			//IL_04d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04db: Expected O, but got Unknown
			//IL_04dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Expected O, but got Unknown
			//IL_04e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Expected O, but got Unknown
			//IL_04f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fc: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			//IL_0529: Unknown result type (might be due to invalid IL or missing references)
			//IL_0533: Expected O, but got Unknown
			//IL_0534: Unknown result type (might be due to invalid IL or missing references)
			//IL_053e: Expected O, but got Unknown
			//IL_053f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0549: Expected O, but got Unknown
			//IL_054a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0554: Expected O, but got Unknown
			//IL_0555: Unknown result type (might be due to invalid IL or missing references)
			//IL_055f: Expected O, but got Unknown
			//IL_0560: Unknown result type (might be due to invalid IL or missing references)
			//IL_056a: Expected O, but got Unknown
			//IL_056b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0575: Expected O, but got Unknown
			//IL_0576: Unknown result type (might be due to invalid IL or missing references)
			//IL_0580: Expected O, but got Unknown
			//IL_0581: Unknown result type (might be due to invalid IL or missing references)
			//IL_058b: Expected O, but got Unknown
			//IL_058c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0596: Expected O, but got Unknown
			//IL_0597: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a1: Expected O, but got Unknown
			//IL_05a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ac: Expected O, but got Unknown
			//IL_05ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b7: Expected O, but got Unknown
			//IL_05b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c2: Expected O, but got Unknown
			//IL_05c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05cd: Expected O, but got Unknown
			//IL_05ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d8: Expected O, but got Unknown
			//IL_05d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e3: Expected O, but got Unknown
			//IL_05e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ee: Expected O, but got Unknown
			//IL_05ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f9: Expected O, but got Unknown
			//IL_05fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0604: Expected O, but got Unknown
			//IL_0605: Unknown result type (might be due to invalid IL or missing references)
			//IL_060f: Expected O, but got Unknown
			//IL_0610: Unknown result type (might be due to invalid IL or missing references)
			//IL_061a: Expected O, but got Unknown
			//IL_061b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0625: Expected O, but got Unknown
			//IL_0626: Unknown result type (might be due to invalid IL or missing references)
			//IL_0630: Expected O, but got Unknown
			//IL_0631: Unknown result type (might be due to invalid IL or missing references)
			//IL_063b: Expected O, but got Unknown
			//IL_063c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0646: Expected O, but got Unknown
			//IL_092e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0938: Expected O, but got Unknown
			//IL_0945: Unknown result type (might be due to invalid IL or missing references)
			//IL_094f: Expected O, but got Unknown
			//IL_17fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_1805: Expected O, but got Unknown
			//IL_27bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_27c7: Expected O, but got Unknown
			//IL_286b: Unknown result type (might be due to invalid IL or missing references)
			//IL_2875: Expected O, but got Unknown
			//IL_2919: Unknown result type (might be due to invalid IL or missing references)
			//IL_2923: Expected O, but got Unknown
			//IL_29c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_29d1: Expected O, but got Unknown
			//IL_5d76: Unknown result type (might be due to invalid IL or missing references)
			//IL_5d80: Expected O, but got Unknown
			//IL_5d88: Unknown result type (might be due to invalid IL or missing references)
			//IL_5d92: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(LightCurve));
			updnScale = new NumericUpDown();
			panelPlot = new Panel();
			picPlot = new PictureBox();
			panelEmailAddresses = new Panel();
			cmdExit_CC = new Button();
			label37 = new Label();
			pictureBox1 = new PictureBox();
			cmdHideEmails = new Button();
			label34 = new Label();
			txtAddresses = new TextBox();
			cmdReLoadCCAddresses = new Button();
			lblEmails = new Label();
			cmdSaveCCAddresses = new Button();
			grpScale = new GroupBox();
			label1 = new Label();
			updnVerticalScaleAdjustment = new NumericUpDown();
			cmd_x15 = new Button();
			cmd_x10 = new Button();
			cmd_x5 = new Button();
			cmd_x1 = new Button();
			label2 = new Label();
			panelComp = new GroupBox();
			label6 = new Label();
			label3 = new Label();
			chkComp2 = new CheckBox();
			chkTarget = new CheckBox();
			chkShowBackground = new CheckBox();
			chkComp1 = new CheckBox();
			chkComp3 = new CheckBox();
			cmbPlotAverage = new ComboBox();
			chkScaleComp = new CheckBox();
			label7 = new Label();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			pasteCSVDataSetToolStripMenuItem = new ToolStripMenuItem();
			openCSVFileToolStripMenuItem = new ToolStripMenuItem();
			correctForMissingExposuresToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			specifyDetailsOfAsteroidalOccultationToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveImageOfThisFormToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem1 = new ToolStripMenuItem();
			setScaleToMatchMonitor100ToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			setEmailAddressesToolStripMenuItem = new ToolStripMenuItem();
			emailMultipleSavedReportsToolStripMenuItem = new ToolStripMenuItem();
			saveEmailReportToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			showFrameTimeInToolTipToolStripMenuItem = new ToolStripMenuItem();
			alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem = new ToolStripMenuItem();
			show_ShortEventEvaluationToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			rightclickToSetFullLightLevelToolStripMenuItem = new ToolStripMenuItem();
			hideToolStripMenuItem = new ToolStripMenuItem();
			toolTip = new ToolTip(components);
			lstComp3 = new ListBox();
			lstComp2 = new ListBox();
			lstComp1 = new ListBox();
			lstTarget = new ListBox();
			cmdIntegrity = new Button();
			cmdIntegration = new Button();
			cmdSetTimeScale = new Button();
			grpRegion = new GroupBox();
			label8 = new Label();
			txtFramesIntegrated = new TextBox();
			label13 = new Label();
			txtNumDataPoints = new TextBox();
			label5 = new Label();
			txtEnd = new TextBox();
			txtStart = new TextBox();
			optEnd = new RadioButton();
			optStart = new RadioButton();
			label4 = new Label();
			label9 = new Label();
			optLunar = new RadioButton();
			optAsteroid = new RadioButton();
			optOther = new RadioButton();
			panel1 = new Panel();
			cmdSpecifyDetails = new Button();
			cmdSaveSend = new Button();
			cmdEmailMultiple = new Button();
			cmdSetEmails = new Button();
			cmdSave = new Button();
			textStar = new TextBox();
			txtObs = new TextBox();
			txtCirc = new TextBox();
			label41 = new Label();
			label42 = new Label();
			label43 = new Label();
			panelControls = new Panel();
			panel2 = new Panel();
			cmdGmailHelp = new Button();
			lblFileName = new Label();
			pnlShort = new Panel();
			lblCopied = new Label();
			lblShortEventWarning = new Label();
			cmdHelp = new Button();
			panel9 = new Panel();
			lblFrame = new Label();
			panel10 = new Panel();
			label10 = new Label();
			lblPossible = new Label();
			label74 = new Label();
			lblProbable = new Label();
			label73 = new Label();
			lblRatio12 = new Label();
			picComp3 = new PictureBox();
			picComp2 = new PictureBox();
			picComp1 = new PictureBox();
			picTarget = new PictureBox();
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
			lblTarget = new Label();
			lblComp1 = new Label();
			lblComp2 = new Label();
			lblComp3 = new Label();
			label69 = new Label();
			lblNoCompStars = new Label();
			label72 = new Label();
			optComp2 = new RadioButton();
			optComp1 = new RadioButton();
			optComp3 = new RadioButton();
			lblHead2 = new Label();
			lblHead3 = new Label();
			lblHead1 = new Label();
			cmdManageReportFiles = new Button();
			((ISupportInitialize)updnScale).BeginInit();
			((Control)panelPlot).SuspendLayout();
			((ISupportInitialize)picPlot).BeginInit();
			((Control)panelEmailAddresses).SuspendLayout();
			((ISupportInitialize)pictureBox1).BeginInit();
			((Control)grpScale).SuspendLayout();
			((ISupportInitialize)updnVerticalScaleAdjustment).BeginInit();
			((Control)panelComp).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpRegion).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panelControls).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)pnlShort).SuspendLayout();
			((Control)panel9).SuspendLayout();
			((Control)panel10).SuspendLayout();
			((ISupportInitialize)picComp3).BeginInit();
			((ISupportInitialize)picComp2).BeginInit();
			((ISupportInitialize)picComp1).BeginInit();
			((ISupportInitialize)picTarget).BeginInit();
			((Control)panel8).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((Control)this).SuspendLayout();
			updnScale.set_DecimalPlaces(1);
			((Control)updnScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnScale.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnScale).set_Location(new Point(69, 13));
			updnScale.set_Maximum(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnScale.set_Minimum(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnScale).set_Name("updnScale");
			((Control)updnScale).set_Size(new Size(42, 20));
			((Control)updnScale).set_TabIndex(1);
			updnScale.set_Value(new decimal(new int[4] { 5, 0, 0, 0 }));
			updnScale.add_ValueChanged((EventHandler)updnScale_ValueChanged);
			((ScrollableControl)panelPlot).set_AutoScroll(true);
			((Control)panelPlot).get_Controls().Add((Control)(object)picPlot);
			((Control)panelPlot).get_Controls().Add((Control)(object)panelEmailAddresses);
			((Control)panelPlot).set_Location(new Point(10, 34));
			((Control)panelPlot).set_Name("panelPlot");
			((Control)panelPlot).set_Size(new Size(987, 316));
			((Control)panelPlot).set_TabIndex(1);
			((Control)picPlot).set_BackColor(Color.Black);
			((Control)picPlot).set_Location(new Point(4, 5));
			((Control)picPlot).set_Name("picPlot");
			((Control)picPlot).set_Size(new Size(978, 308));
			picPlot.set_TabIndex(0);
			picPlot.set_TabStop(false);
			((Control)picPlot).add_MouseClick(new MouseEventHandler(picPlot_MouseClick));
			((Control)picPlot).add_MouseMove(new MouseEventHandler(picPlot_MouseMove));
			panelEmailAddresses.set_BorderStyle((BorderStyle)2);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)cmdExit_CC);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)label37);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)pictureBox1);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)cmdHideEmails);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)label34);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)txtAddresses);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)cmdReLoadCCAddresses);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)lblEmails);
			((Control)panelEmailAddresses).get_Controls().Add((Control)(object)cmdSaveCCAddresses);
			((Control)panelEmailAddresses).set_Location(new Point(542, 45));
			((Control)panelEmailAddresses).set_Name("panelEmailAddresses");
			((Control)panelEmailAddresses).set_Size(new Size(424, 245));
			((Control)panelEmailAddresses).set_TabIndex(1);
			((Control)panelEmailAddresses).set_Visible(false);
			((Control)cmdExit_CC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdExit_CC).set_Location(new Point(294, 204));
			((Control)cmdExit_CC).set_Name("cmdExit_CC");
			((Control)cmdExit_CC).set_Size(new Size(57, 26));
			((Control)cmdExit_CC).set_TabIndex(46);
			((Control)cmdExit_CC).set_Text("Exit");
			((ButtonBase)cmdExit_CC).set_UseVisualStyleBackColor(true);
			((Control)cmdExit_CC).add_Click((EventHandler)cmdExit_CC_Click);
			((Control)label37).set_Location(new Point(39, 39));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(343, 27));
			((Control)label37).set_TabIndex(45);
			((Control)label37).set_Text("EMail addresses specified here will be included as a CC address \r\nwhenever a Light Curve report is EMailed.");
			label37.set_TextAlign(ContentAlignment.TopCenter);
			pictureBox1.set_Image((Image)Resources.UtilityText);
			((Control)pictureBox1).set_Location(new Point(-2, 3));
			((Control)pictureBox1).set_Name("pictureBox1");
			((Control)pictureBox1).set_Size(new Size(30, 34));
			pictureBox1.set_TabIndex(44);
			pictureBox1.set_TabStop(false);
			((ButtonBase)cmdHideEmails).set_Image((Image)Resources.error);
			((Control)cmdHideEmails).set_Location(new Point(390, 5));
			((Control)cmdHideEmails).set_Name("cmdHideEmails");
			((Control)cmdHideEmails).set_Size(new Size(22, 20));
			((Control)cmdHideEmails).set_TabIndex(43);
			((ButtonBase)cmdHideEmails).set_UseVisualStyleBackColor(true);
			((Control)cmdHideEmails).add_Click((EventHandler)cmdHideEmails_Click);
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(35, 9));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(354, 17));
			((Control)label34).set_TabIndex(42);
			((Control)label34).set_Text("Edit email CC addresses for Light Curve reports");
			((TextBoxBase)txtAddresses).set_BorderStyle((BorderStyle)1);
			((Control)txtAddresses).set_Location(new Point(14, 109));
			((TextBoxBase)txtAddresses).set_Multiline(true);
			((Control)txtAddresses).set_Name("txtAddresses");
			txtAddresses.set_ScrollBars((ScrollBars)2);
			((Control)txtAddresses).set_Size(new Size(398, 92));
			((Control)txtAddresses).set_TabIndex(14);
			((Control)txtAddresses).add_TextChanged((EventHandler)txtAddresses_TextChanged);
			((Control)cmdReLoadCCAddresses).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReLoadCCAddresses).set_Location(new Point(182, 205));
			((Control)cmdReLoadCCAddresses).set_Name("cmdReLoadCCAddresses");
			((Control)cmdReLoadCCAddresses).set_Size(new Size(57, 26));
			((Control)cmdReLoadCCAddresses).set_TabIndex(41);
			((Control)cmdReLoadCCAddresses).set_Text("Re-load");
			((ButtonBase)cmdReLoadCCAddresses).set_UseVisualStyleBackColor(true);
			((Control)cmdReLoadCCAddresses).add_Click((EventHandler)cmdReLoadCCAddresses_Click);
			((Control)lblEmails).set_AutoSize(true);
			((Control)lblEmails).set_Location(new Point(40, 75));
			((Control)lblEmails).set_Name("lblEmails");
			((Control)lblEmails).set_Size(new Size(346, 26));
			((Control)lblEmails).set_TabIndex(39);
			((Control)lblEmails).set_Text("Email addresses.   Separate with a semicolon (  ;  ). Can use labels, as in\r\nJohn Smith <john.smith@example.org>\r\n");
			lblEmails.set_TextAlign(ContentAlignment.TopCenter);
			((Control)cmdSaveCCAddresses).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSaveCCAddresses).set_Location(new Point(70, 205));
			((Control)cmdSaveCCAddresses).set_Name("cmdSaveCCAddresses");
			((Control)cmdSaveCCAddresses).set_Size(new Size(57, 27));
			((Control)cmdSaveCCAddresses).set_TabIndex(40);
			((Control)cmdSaveCCAddresses).set_Text("Save");
			((ButtonBase)cmdSaveCCAddresses).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveCCAddresses).add_Click((EventHandler)cmdSaveCCAddresses_Click);
			((Control)grpScale).get_Controls().Add((Control)(object)label1);
			((Control)grpScale).get_Controls().Add((Control)(object)updnVerticalScaleAdjustment);
			((Control)grpScale).get_Controls().Add((Control)(object)cmd_x15);
			((Control)grpScale).get_Controls().Add((Control)(object)cmd_x10);
			((Control)grpScale).get_Controls().Add((Control)(object)cmd_x5);
			((Control)grpScale).get_Controls().Add((Control)(object)cmd_x1);
			((Control)grpScale).get_Controls().Add((Control)(object)updnScale);
			((Control)grpScale).get_Controls().Add((Control)(object)label2);
			((Control)grpScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpScale).set_Location(new Point(99, 3));
			((Control)grpScale).set_Name("grpScale");
			((Control)grpScale).set_Size(new Size(121, 91));
			((Control)grpScale).set_TabIndex(3);
			grpScale.set_TabStop(false);
			((Control)grpScale).set_Text("Plot scale");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(7, 17));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(54, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Horizontal");
			updnVerticalScaleAdjustment.set_DecimalPlaces(2);
			((Control)updnVerticalScaleAdjustment).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnVerticalScaleAdjustment.set_Increment(new decimal(new int[4] { 2, 0, 0, 131072 }));
			((Control)updnVerticalScaleAdjustment).set_Location(new Point(52, 66));
			updnVerticalScaleAdjustment.set_Maximum(new decimal(new int[4] { 15, 0, 0, 65536 }));
			updnVerticalScaleAdjustment.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnVerticalScaleAdjustment).set_Name("updnVerticalScaleAdjustment");
			((Control)updnVerticalScaleAdjustment).set_Size(new Size(42, 20));
			((Control)updnVerticalScaleAdjustment).set_TabIndex(7);
			updnVerticalScaleAdjustment.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnVerticalScaleAdjustment.add_ValueChanged((EventHandler)updnVerticalScaleAdjustment_ValueChanged_1);
			((Control)cmd_x15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x15).set_Location(new Point(82, 40));
			((Control)cmd_x15).set_Name("cmd_x15");
			((Control)cmd_x15).set_Size(new Size(31, 20));
			((Control)cmd_x15).set_TabIndex(5);
			((Control)cmd_x15).set_Text("15");
			((ButtonBase)cmd_x15).set_UseVisualStyleBackColor(true);
			((Control)cmd_x15).add_Click((EventHandler)cmd_x15_Click);
			((Control)cmd_x10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x10).set_Location(new Point(53, 40));
			((Control)cmd_x10).set_Name("cmd_x10");
			((Control)cmd_x10).set_Size(new Size(27, 20));
			((Control)cmd_x10).set_TabIndex(4);
			((Control)cmd_x10).set_Text("10");
			((ButtonBase)cmd_x10).set_UseVisualStyleBackColor(true);
			((Control)cmd_x10).add_Click((EventHandler)cmd_x10_Click);
			((Control)cmd_x5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x5).set_Location(new Point(29, 40));
			((Control)cmd_x5).set_Name("cmd_x5");
			((Control)cmd_x5).set_Size(new Size(22, 20));
			((Control)cmd_x5).set_TabIndex(3);
			((Control)cmd_x5).set_Text("5");
			((ButtonBase)cmd_x5).set_UseVisualStyleBackColor(true);
			((Control)cmd_x5).add_Click((EventHandler)cmd_x5_Click);
			((Control)cmd_x1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x1).set_Location(new Point(5, 40));
			((Control)cmd_x1).set_Name("cmd_x1");
			((Control)cmd_x1).set_Size(new Size(22, 20));
			((Control)cmd_x1).set_TabIndex(2);
			((Control)cmd_x1).set_Text("1");
			((ButtonBase)cmd_x1).set_UseVisualStyleBackColor(true);
			((Control)cmd_x1).add_Click((EventHandler)cmd_x1_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(7, 70));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(42, 13));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Vertical");
			((Control)panelComp).get_Controls().Add((Control)(object)label6);
			((Control)panelComp).get_Controls().Add((Control)(object)label3);
			((Control)panelComp).get_Controls().Add((Control)(object)chkComp2);
			((Control)panelComp).get_Controls().Add((Control)(object)chkTarget);
			((Control)panelComp).get_Controls().Add((Control)(object)chkShowBackground);
			((Control)panelComp).get_Controls().Add((Control)(object)chkComp1);
			((Control)panelComp).get_Controls().Add((Control)(object)chkComp3);
			((Control)panelComp).get_Controls().Add((Control)(object)cmbPlotAverage);
			((Control)panelComp).get_Controls().Add((Control)(object)chkScaleComp);
			((Control)panelComp).get_Controls().Add((Control)(object)label7);
			((Control)panelComp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)panelComp).set_Location(new Point(4, 3));
			((Control)panelComp).set_Name("panelComp");
			((Control)panelComp).set_Size(new Size(86, 206));
			((Control)panelComp).set_TabIndex(2);
			panelComp.set_TabStop(false);
			((Control)panelComp).set_Text("Plot control");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(1, 13));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(84, 13));
			((Control)label6).set_TabIndex(0);
			((Control)label6).set_Text("Include lines for:");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_ForeColor(Color.Fuchsia);
			((Control)label3).set_Location(new Point(46, 184));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(36, 13));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("- point");
			((Control)chkComp2).set_AutoSize(true);
			((Control)chkComp2).set_BackColor(Color.Orange);
			chkComp2.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkComp2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkComp2).set_ForeColor(Color.Black);
			((Control)chkComp2).set_Location(new Point(16, 69));
			((Control)chkComp2).set_Name("chkComp2");
			((Control)chkComp2).set_Size(new Size(62, 17));
			((Control)chkComp2).set_TabIndex(3);
			((Control)chkComp2).set_Text("Comp 2");
			((ButtonBase)chkComp2).set_UseVisualStyleBackColor(false);
			chkComp2.add_CheckedChanged((EventHandler)chkComp2_CheckedChanged);
			((Control)chkTarget).set_AutoSize(true);
			chkTarget.set_CheckAlign(ContentAlignment.MiddleRight);
			chkTarget.set_Checked(Settings.Default.LightCurve_LinesForTarget);
			chkTarget.set_CheckState((CheckState)1);
			((Control)chkTarget).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LightCurve_LinesForTarget", true, (DataSourceUpdateMode)1));
			((Control)chkTarget).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkTarget).set_ForeColor(Color.Blue);
			((Control)chkTarget).set_Location(new Point(19, 29));
			((Control)chkTarget).set_Name("chkTarget");
			((Control)chkTarget).set_Size(new Size(57, 17));
			((Control)chkTarget).set_TabIndex(1);
			((Control)chkTarget).set_Text("Target");
			((ButtonBase)chkTarget).set_UseVisualStyleBackColor(true);
			chkTarget.add_CheckedChanged((EventHandler)chkTarget_CheckedChanged);
			((Control)chkShowBackground).set_AutoSize(true);
			((Control)chkShowBackground).set_BackColor(Color.Peru);
			chkShowBackground.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkShowBackground).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShowBackground).set_ForeColor(Color.Black);
			((Control)chkShowBackground).set_Location(new Point(6, 142));
			((Control)chkShowBackground).set_Name("chkShowBackground");
			((Control)chkShowBackground).set_Size(new Size(72, 17));
			((Control)chkShowBackground).set_TabIndex(5);
			((Control)chkShowBackground).set_Text("Backgrnd");
			((ButtonBase)chkShowBackground).set_UseVisualStyleBackColor(false);
			chkShowBackground.add_CheckedChanged((EventHandler)chkShowBackground_CheckedChanged);
			((Control)chkComp1).set_AutoSize(true);
			((Control)chkComp1).set_BackColor(Color.LimeGreen);
			chkComp1.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkComp1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkComp1).set_ForeColor(Color.Black);
			((Control)chkComp1).set_Location(new Point(16, 49));
			((Control)chkComp1).set_Name("chkComp1");
			((Control)chkComp1).set_Size(new Size(62, 17));
			((Control)chkComp1).set_TabIndex(2);
			((Control)chkComp1).set_Text("Comp 1");
			((ButtonBase)chkComp1).set_UseVisualStyleBackColor(false);
			chkComp1.add_CheckedChanged((EventHandler)chkComp1_CheckedChanged);
			((Control)chkComp3).set_AutoSize(true);
			((Control)chkComp3).set_BackColor(Color.FromArgb(255, 100, 100));
			chkComp3.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkComp3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkComp3).set_ForeColor(Color.Black);
			((Control)chkComp3).set_Location(new Point(16, 89));
			((Control)chkComp3).set_Name("chkComp3");
			((Control)chkComp3).set_Size(new Size(62, 17));
			((Control)chkComp3).set_TabIndex(4);
			((Control)chkComp3).set_Text("Comp 3");
			((ButtonBase)chkComp3).set_UseVisualStyleBackColor(false);
			chkComp3.add_CheckedChanged((EventHandler)chkComp3_CheckedChanged);
			((Control)cmbPlotAverage).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPlotAverage).set_FormattingEnabled(true);
			cmbPlotAverage.get_Items().AddRange(new object[8] { "0", "3", "5", "7", "9", "15", "25", "35" });
			((Control)cmbPlotAverage).set_Location(new Point(6, 180));
			((Control)cmbPlotAverage).set_Name("cmbPlotAverage");
			((Control)cmbPlotAverage).set_Size(new Size(37, 21));
			((Control)cmbPlotAverage).set_TabIndex(7);
			cmbPlotAverage.add_SelectedIndexChanged((EventHandler)cmbPlotAverage_SelectedIndexChanged);
			((Control)chkScaleComp).set_AutoSize(true);
			((Control)chkScaleComp).set_BackColor(Color.Aquamarine);
			chkScaleComp.set_CheckAlign(ContentAlignment.MiddleRight);
			chkScaleComp.set_Checked(true);
			chkScaleComp.set_CheckState((CheckState)1);
			((Control)chkScaleComp).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkScaleComp).set_ForeColor(Color.DarkRed);
			((Control)chkScaleComp).set_Location(new Point(4, 109));
			((Control)chkScaleComp).set_Name("chkScaleComp");
			((Control)chkScaleComp).set_Size(new Size(78, 30));
			((Control)chkScaleComp).set_TabIndex(9);
			((Control)chkScaleComp).set_Text("Vert. scale\r\nComp stars");
			((ButtonBase)chkScaleComp).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)chkScaleComp).set_UseVisualStyleBackColor(false);
			chkScaleComp.add_CheckedChanged((EventHandler)chkScaleComp_CheckedChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(3, 165));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(69, 13));
			((Control)label7).set_TabIndex(6);
			((Control)label7).set_Text("Running Ave");
			label7.set_TextAlign(ContentAlignment.TopCenter);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)show_ShortEventEvaluationToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)rightclickToSetFullLightLevelToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1026, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[17]
			{
				(ToolStripItem)pasteCSVDataSetToolStripMenuItem,
				(ToolStripItem)openCSVFileToolStripMenuItem,
				(ToolStripItem)correctForMissingExposuresToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveImageOfThisFormToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)setEmailAddressesToolStripMenuItem,
				(ToolStripItem)emailMultipleSavedReportsToolStripMenuItem,
				(ToolStripItem)saveEmailReportToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)showFrameTimeInToolTipToolStripMenuItem,
				(ToolStripItem)alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(58, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...    ");
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Image((Image)Resources.PasteHS);
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Name("pasteCSVDataSetToolStripMenuItem");
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).set_Text("Paste CSV data set ");
			((ToolStripItem)pasteCSVDataSetToolStripMenuItem).add_Click((EventHandler)pasteCSVDataSetToolStripMenuItem_Click);
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Image((Image)Resources.OpenSelectedItemHS);
			((ToolStripItem)openCSVFileToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Name("openCSVFileToolStripMenuItem");
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)openCSVFileToolStripMenuItem).set_Text("Open CSV file");
			((ToolStripItem)openCSVFileToolStripMenuItem).add_Click((EventHandler)openCSVFileToolStripMenuItem_Click);
			correctForMissingExposuresToolStripMenuItem.set_Checked(true);
			correctForMissingExposuresToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).set_Name("correctForMissingExposuresToolStripMenuItem");
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).set_Text("Correct for missing exposures");
			((ToolStripItem)correctForMissingExposuresToolStripMenuItem).add_Click((EventHandler)correctForMissingExposuresToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(316, 6));
			((ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem).set_Name("specifyDetailsOfAsteroidalOccultationToolStripMenuItem");
			((ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem).set_Text("Specify details of asteroidal occultation");
			((ToolStripItem)specifyDetailsOfAsteroidalOccultationToolStripMenuItem).add_Click((EventHandler)specifyDetailsOfAsteroidalOccultationToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(316, 6));
			((ToolStripItem)saveToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save _this_ report");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)saveImageOfThisFormToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)saveToolStripMenuItem1,
				(ToolStripItem)setScaleToMatchMonitor100ToolStripMenuItem
			});
			((ToolStripItem)saveImageOfThisFormToolStripMenuItem).set_Image((Image)Resources.SaveTable_16x_32);
			((ToolStripItem)saveImageOfThisFormToolStripMenuItem).set_Name("saveImageOfThisFormToolStripMenuItem");
			((ToolStripItem)saveImageOfThisFormToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)saveImageOfThisFormToolStripMenuItem).set_Text("Save image of this form");
			((ToolStripItem)saveToolStripMenuItem1).set_Name("saveToolStripMenuItem1");
			((ToolStripItem)saveToolStripMenuItem1).set_Size(new Size(334, 22));
			((ToolStripItem)saveToolStripMenuItem1).set_Text("Save image of form");
			((ToolStripItem)saveToolStripMenuItem1).add_Click((EventHandler)saveToolStripMenuItem1_Click);
			((ToolStripItem)setScaleToMatchMonitor100ToolStripMenuItem).set_Name("setScaleToMatchMonitor100ToolStripMenuItem");
			((ToolStripItem)setScaleToMatchMonitor100ToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)setScaleToMatchMonitor100ToolStripMenuItem).set_Text("Set scale (currently 100%) to match Monitor scale");
			((ToolStripItem)setScaleToMatchMonitor100ToolStripMenuItem).add_Click((EventHandler)setScaleToMatchMonitor100ToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(316, 6));
			((ToolStripItem)setEmailAddressesToolStripMenuItem).set_Image((Image)Resources.UtilityText);
			((ToolStripItem)setEmailAddressesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)setEmailAddressesToolStripMenuItem).set_Name("setEmailAddressesToolStripMenuItem");
			((ToolStripItem)setEmailAddressesToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)setEmailAddressesToolStripMenuItem).set_Text("Set CC email addresses for Light Curves");
			((ToolStripItem)setEmailAddressesToolStripMenuItem).add_Click((EventHandler)setEmailAddressesToolStripMenuItem_Click);
			((ToolStripItem)emailMultipleSavedReportsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)emailMultipleSavedReportsToolStripMenuItem).set_Image((Image)Resources.MAIL21A);
			((ToolStripItem)emailMultipleSavedReportsToolStripMenuItem).set_Name("emailMultipleSavedReportsToolStripMenuItem");
			((ToolStripItem)emailMultipleSavedReportsToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)emailMultipleSavedReportsToolStripMenuItem).set_Text("Email multiple saved reports");
			((ToolStripItem)emailMultipleSavedReportsToolStripMenuItem).add_Click((EventHandler)emailMultipleSavedReportsToolStripMenuItem_Click);
			((ToolStripItem)saveEmailReportToolStripMenuItem).set_Image((Image)Resources.MAIL21A);
			((ToolStripItem)saveEmailReportToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveEmailReportToolStripMenuItem).set_Name("saveEmailReportToolStripMenuItem");
			((ToolStripItem)saveEmailReportToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)saveEmailReportToolStripMenuItem).set_Text("Save && Email _this_ report");
			((ToolStripItem)saveEmailReportToolStripMenuItem).add_Click((EventHandler)SaveSendEmailReportToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(316, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy report");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(316, 6));
			((ToolStripItem)showFrameTimeInToolTipToolStripMenuItem).set_Image((Image)Resources.CLOCK02);
			((ToolStripItem)showFrameTimeInToolTipToolStripMenuItem).set_Name("showFrameTimeInToolTipToolStripMenuItem");
			((ToolStripItem)showFrameTimeInToolTipToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)showFrameTimeInToolTipToolStripMenuItem).set_Text("Show frame && time in ToolTip");
			((ToolStripItem)showFrameTimeInToolTipToolStripMenuItem).add_Click((EventHandler)showFrameTimeInToolTipToolStripMenuItem_Click);
			alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem.set_Checked(Settings.Default.AOTAshowComparisons);
			alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem).set_Name("alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem");
			((ToolStripItem)alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem).set_Size(new Size(319, 22));
			((ToolStripItem)alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem).set_Text("Always show comparisons stars with File Open");
			((ToolStripItem)alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem).add_Click((EventHandler)alwaysShowComparisonsStarsWithFileOpenToolStripMenuItem_Click);
			((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).set_Name("show_ShortEventEvaluationToolStripMenuItem");
			((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).set_Size(new Size(178, 20));
			((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).set_Text("Show Short event evaluation   ");
			((ToolStripItem)show_ShortEventEvaluationToolStripMenuItem).add_Click((EventHandler)show_ShortEventEvaluationToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(56, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(102, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit                ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripItem)rightclickToSetFullLightLevelToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)rightclickToSetFullLightLevelToolStripMenuItem).set_ForeColor(Color.Red);
			((ToolStripItem)rightclickToSetFullLightLevelToolStripMenuItem).set_Name("rightclickToSetFullLightLevelToolStripMenuItem");
			((ToolStripItem)rightclickToSetFullLightLevelToolStripMenuItem).set_Size(new Size(182, 20));
			((ToolStripItem)rightclickToSetFullLightLevelToolStripMenuItem).set_Text("Right-click to set full light level");
			((ToolStripItem)hideToolStripMenuItem).set_Name("hideToolStripMenuItem");
			((ToolStripItem)hideToolStripMenuItem).set_Size(new Size(32, 19));
			toolTip.set_AutomaticDelay(100);
			toolTip.set_AutoPopDelay(300);
			toolTip.set_InitialDelay(100);
			toolTip.set_ReshowDelay(20);
			((Control)lstComp3).set_BackColor(Color.LightCyan);
			((Control)lstComp3).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstComp3).set_FormattingEnabled(true);
			((Control)lstComp3).set_Location(new Point(572, 95));
			((Control)lstComp3).set_Name("lstComp3");
			((Control)lstComp3).set_Size(new Size(109, 82));
			((Control)lstComp3).set_TabIndex(50);
			toolTip.SetToolTip((Control)(object)lstComp3, "Right click to copy");
			((Control)lstComp3).add_MouseDown(new MouseEventHandler(lstComp3_MouseDown));
			((Control)lstComp2).set_BackColor(Color.LightCyan);
			((Control)lstComp2).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstComp2).set_FormattingEnabled(true);
			((Control)lstComp2).set_Location(new Point(431, 95));
			((Control)lstComp2).set_Name("lstComp2");
			((Control)lstComp2).set_Size(new Size(109, 82));
			((Control)lstComp2).set_TabIndex(49);
			toolTip.SetToolTip((Control)(object)lstComp2, "Right click to copy");
			((Control)lstComp2).add_MouseDown(new MouseEventHandler(lstComp2_MouseDown));
			((Control)lstComp1).set_BackColor(Color.LightCyan);
			((Control)lstComp1).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstComp1).set_FormattingEnabled(true);
			((Control)lstComp1).set_Location(new Point(290, 95));
			((Control)lstComp1).set_Name("lstComp1");
			((Control)lstComp1).set_Size(new Size(109, 82));
			((Control)lstComp1).set_TabIndex(48);
			toolTip.SetToolTip((Control)(object)lstComp1, "Right click to copy");
			((Control)lstComp1).add_MouseDown(new MouseEventHandler(lstComp1_MouseDown));
			((Control)lstTarget).set_BackColor(Color.Ivory);
			((Control)lstTarget).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstTarget).set_FormattingEnabled(true);
			((Control)lstTarget).set_Location(new Point(11, 95));
			((Control)lstTarget).set_Name("lstTarget");
			((Control)lstTarget).set_Size(new Size(250, 82));
			((Control)lstTarget).set_TabIndex(47);
			toolTip.SetToolTip((Control)(object)lstTarget, "Right click to copy");
			((Control)lstTarget).add_MouseDown(new MouseEventHandler(lstTarget_MouseDown));
			((Control)cmdIntegrity).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdIntegrity).set_Location(new Point(98, 98));
			((Control)cmdIntegrity).set_Name("cmdIntegrity");
			((Control)cmdIntegrity).set_Size(new Size(102, 25));
			((Control)cmdIntegrity).set_TabIndex(4);
			((Control)cmdIntegrity).set_Text("Integrity check");
			((ButtonBase)cmdIntegrity).set_UseVisualStyleBackColor(true);
			((Control)cmdIntegrity).add_Click((EventHandler)cmdIntegrity_Click);
			((Control)cmdIntegration).set_BackColor(Color.Ivory);
			((Control)cmdIntegration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdIntegration).set_Location(new Point(98, 148));
			((Control)cmdIntegration).set_Name("cmdIntegration");
			((Control)cmdIntegration).set_Size(new Size(102, 61));
			((Control)cmdIntegration).set_TabIndex(6);
			((Control)cmdIntegration).set_Text("* Background\r\n* Integration\r\n* Binning\r\n* Normalisation");
			((ButtonBase)cmdIntegration).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdIntegration).set_UseVisualStyleBackColor(false);
			((Control)cmdIntegration).add_Click((EventHandler)cmdIntegration_Click);
			((Control)cmdSetTimeScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSetTimeScale).set_Location(new Point(98, 123));
			((Control)cmdSetTimeScale).set_Name("cmdSetTimeScale");
			((Control)cmdSetTimeScale).set_Size(new Size(102, 25));
			((Control)cmdSetTimeScale).set_TabIndex(5);
			((Control)cmdSetTimeScale).set_Text("Set time scale");
			((ButtonBase)cmdSetTimeScale).set_UseVisualStyleBackColor(true);
			((Control)cmdSetTimeScale).add_Click((EventHandler)cmdSetTimeScale_Click);
			((Control)grpRegion).get_Controls().Add((Control)(object)label8);
			((Control)grpRegion).get_Controls().Add((Control)(object)txtFramesIntegrated);
			((Control)grpRegion).get_Controls().Add((Control)(object)label13);
			((Control)grpRegion).get_Controls().Add((Control)(object)txtNumDataPoints);
			((Control)grpRegion).get_Controls().Add((Control)(object)label5);
			((Control)grpRegion).get_Controls().Add((Control)(object)txtEnd);
			((Control)grpRegion).get_Controls().Add((Control)(object)txtStart);
			((Control)grpRegion).get_Controls().Add((Control)(object)optEnd);
			((Control)grpRegion).get_Controls().Add((Control)(object)optStart);
			((Control)grpRegion).get_Controls().Add((Control)(object)label4);
			((Control)grpRegion).get_Controls().Add((Control)(object)label9);
			((Control)grpRegion).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpRegion).set_Location(new Point(227, 3));
			((Control)grpRegion).set_Name("grpRegion");
			((Control)grpRegion).set_Size(new Size(128, 206));
			((Control)grpRegion).set_TabIndex(7);
			grpRegion.set_TabStop(false);
			((Control)grpRegion).set_Text("Set region to archive");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(27, 134));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(41, 13));
			((Control)label8).set_TabIndex(8);
			((Control)label8).set_Text("binning");
			((Control)txtFramesIntegrated).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFramesIntegrated).set_Location(new Point(70, 130));
			((Control)txtFramesIntegrated).set_Name("txtFramesIntegrated");
			((TextBoxBase)txtFramesIntegrated).set_ReadOnly(true);
			((Control)txtFramesIntegrated).set_Size(new Size(20, 20));
			((Control)txtFramesIntegrated).set_TabIndex(9);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(13, 110));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(55, 13));
			((Control)label13).set_TabIndex(5);
			((Control)label13).set_Text("# data pts");
			((Control)txtNumDataPoints).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtNumDataPoints).set_Location(new Point(70, 106));
			((Control)txtNumDataPoints).set_Name("txtNumDataPoints");
			((TextBoxBase)txtNumDataPoints).set_ReadOnly(true);
			((Control)txtNumDataPoints).set_Size(new Size(40, 20));
			((Control)txtNumDataPoints).set_TabIndex(6);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(46, 65));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(36, 13));
			((Control)label5).set_TabIndex(2);
			((Control)label5).set_Text("Frame");
			((Control)txtEnd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEnd).set_Location(new Point(70, 81));
			((Control)txtEnd).set_Name("txtEnd");
			((TextBoxBase)txtEnd).set_ReadOnly(true);
			((Control)txtEnd).set_Size(new Size(46, 20));
			((Control)txtEnd).set_TabIndex(4);
			((Control)txtStart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStart).set_Location(new Point(12, 81));
			((Control)txtStart).set_Name("txtStart");
			((TextBoxBase)txtStart).set_ReadOnly(true);
			((Control)txtStart).set_Size(new Size(46, 20));
			((Control)txtStart).set_TabIndex(3);
			((Control)optEnd).set_AutoSize(true);
			optEnd.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)optEnd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optEnd).set_Location(new Point(78, 36));
			((Control)optEnd).set_Name("optEnd");
			((Control)optEnd).set_Size(new Size(30, 30));
			((Control)optEnd).set_TabIndex(1);
			((Control)optEnd).set_Text("End");
			((ButtonBase)optEnd).set_UseVisualStyleBackColor(true);
			((Control)optStart).set_AutoSize(true);
			optStart.set_CheckAlign(ContentAlignment.BottomCenter);
			optStart.set_Checked(true);
			((Control)optStart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optStart).set_Location(new Point(19, 36));
			((Control)optStart).set_Name("optStart");
			((Control)optStart).set_Size(new Size(33, 30));
			((Control)optStart).set_TabIndex(0);
			optStart.set_TabStop(true);
			((Control)optStart).set_Text("Start");
			((ButtonBase)optStart).set_UseVisualStyleBackColor(true);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(6, 158));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(110, 39));
			((Control)label4).set_TabIndex(7);
			((Control)label4).set_Text("To set, click Mouse \r\nat the start point, then\r\nthe end point");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(89, 134));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(38, 13));
			((Control)label9).set_TabIndex(10);
			((Control)label9).set_Text("frames");
			((Control)optLunar).set_AutoSize(true);
			optLunar.set_Checked(true);
			((Control)optLunar).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optLunar).set_Location(new Point(13, 4));
			((Control)optLunar).set_Name("optLunar");
			((Control)optLunar).set_Size(new Size(68, 21));
			((Control)optLunar).set_TabIndex(0);
			optLunar.set_TabStop(true);
			((Control)optLunar).set_Text("Lunar");
			((ButtonBase)optLunar).set_UseVisualStyleBackColor(true);
			optLunar.add_CheckedChanged((EventHandler)optLunar_CheckedChanged);
			((Control)optAsteroid).set_AutoSize(true);
			((Control)optAsteroid).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optAsteroid).set_Location(new Point(89, 4));
			((Control)optAsteroid).set_Name("optAsteroid");
			((Control)optAsteroid).set_Size(new Size(86, 21));
			((Control)optAsteroid).set_TabIndex(1);
			((Control)optAsteroid).set_Text("Asteroid");
			((ButtonBase)optAsteroid).set_UseVisualStyleBackColor(true);
			optAsteroid.add_CheckedChanged((EventHandler)optAsteroid_CheckedChanged);
			((Control)optOther).set_AutoSize(true);
			((Control)optOther).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optOther).set_Location(new Point(183, 6));
			((Control)optOther).set_Name("optOther");
			((Control)optOther).set_Size(new Size(56, 17));
			((Control)optOther).set_TabIndex(2);
			((Control)optOther).set_Text("Other");
			((ButtonBase)optOther).set_UseVisualStyleBackColor(true);
			((Control)optOther).set_Visible(false);
			optOther.add_CheckedChanged((EventHandler)optOther_CheckedChanged);
			((Control)panel1).get_Controls().Add((Control)(object)optOther);
			((Control)panel1).get_Controls().Add((Control)(object)optAsteroid);
			((Control)panel1).get_Controls().Add((Control)(object)optLunar);
			((Control)panel1).set_Location(new Point(361, 2));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(246, 28));
			((Control)panel1).set_TabIndex(9);
			((Control)cmdSpecifyDetails).set_BackColor(Color.PaleGreen);
			((ButtonBase)cmdSpecifyDetails).get_FlatAppearance().set_BorderColor(Color.Red);
			((ButtonBase)cmdSpecifyDetails).set_FlatStyle((FlatStyle)0);
			((Control)cmdSpecifyDetails).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSpecifyDetails).set_Location(new Point(616, 2));
			((Control)cmdSpecifyDetails).set_Name("cmdSpecifyDetails");
			((Control)cmdSpecifyDetails).set_Size(new Size(178, 29));
			((Control)cmdSpecifyDetails).set_TabIndex(8);
			((Control)cmdSpecifyDetails).set_Text("Set event && observer details");
			((ButtonBase)cmdSpecifyDetails).set_UseVisualStyleBackColor(false);
			((Control)cmdSpecifyDetails).add_Click((EventHandler)cmdSpecifyDetails_Click);
			((Control)cmdSaveSend).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSaveSend).set_Location(new Point(856, 20));
			((Control)cmdSaveSend).set_Name("cmdSaveSend");
			((Control)cmdSaveSend).set_Size(new Size(120, 35));
			((Control)cmdSaveSend).set_TabIndex(13);
			((Control)cmdSaveSend).set_Text("Save && Email this report");
			((ButtonBase)cmdSaveSend).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveSend).add_Click((EventHandler)cmdSaveSend_Click);
			((Control)cmdEmailMultiple).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdEmailMultiple).set_Location(new Point(1, 27));
			((Control)cmdEmailMultiple).set_Name("cmdEmailMultiple");
			((Control)cmdEmailMultiple).set_Size(new Size(120, 35));
			((Control)cmdEmailMultiple).set_TabIndex(14);
			((Control)cmdEmailMultiple).set_Text("Email multiple \r\nsaved reports");
			((ButtonBase)cmdEmailMultiple).set_UseVisualStyleBackColor(true);
			((Control)cmdEmailMultiple).add_Click((EventHandler)cmdEmailMultiple_Click);
			((Control)cmdSetEmails).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSetEmails).set_Location(new Point(856, 160));
			((Control)cmdSetEmails).set_Name("cmdSetEmails");
			((Control)cmdSetEmails).set_Size(new Size(120, 29));
			((Control)cmdSetEmails).set_TabIndex(15);
			((Control)cmdSetEmails).set_Text("Set CC addresses");
			((ButtonBase)cmdSetEmails).set_UseVisualStyleBackColor(true);
			((Control)cmdSetEmails).add_Click((EventHandler)cmdSetEmails_Click);
			((Control)cmdSave).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSave).set_Location(new Point(1, 2));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(120, 24));
			((Control)cmdSave).set_TabIndex(16);
			((Control)cmdSave).set_Text("Save this report");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(true);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((Control)textStar).set_BackColor(Color.FloralWhite);
			((Control)textStar).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textStar).set_Location(new Point(361, 47));
			((TextBoxBase)textStar).set_Multiline(true);
			((Control)textStar).set_Name("textStar");
			((TextBoxBase)textStar).set_ReadOnly(true);
			((Control)textStar).set_Size(new Size(123, 160));
			((Control)textStar).set_TabIndex(35);
			((Control)txtObs).set_BackColor(Color.FloralWhite);
			((Control)txtObs).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObs).set_Location(new Point(490, 47));
			((TextBoxBase)txtObs).set_Multiline(true);
			((Control)txtObs).set_Name("txtObs");
			((TextBoxBase)txtObs).set_ReadOnly(true);
			((Control)txtObs).set_Size(new Size(181, 160));
			((Control)txtObs).set_TabIndex(36);
			((Control)txtCirc).set_BackColor(Color.FloralWhite);
			((Control)txtCirc).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCirc).set_Location(new Point(677, 47));
			((TextBoxBase)txtCirc).set_Multiline(true);
			((Control)txtCirc).set_Name("txtCirc");
			((TextBoxBase)txtCirc).set_ReadOnly(true);
			((Control)txtCirc).set_Size(new Size(163, 160));
			((Control)txtCirc).set_TabIndex(37);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(490, 33));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(114, 13));
			((Control)label41).set_TabIndex(40);
			((Control)label41).set_Text("Date and Observer");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(677, 33));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(89, 13));
			((Control)label42).set_TabIndex(39);
			((Control)label42).set_Text("Circumstances");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(364, 33));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(30, 13));
			((Control)label43).set_TabIndex(38);
			((Control)label43).set_Text("Star");
			((Control)panelControls).set_BackColor(Color.Honeydew);
			panelControls.set_BorderStyle((BorderStyle)2);
			((Control)panelControls).get_Controls().Add((Control)(object)txtCirc);
			((Control)panelControls).get_Controls().Add((Control)(object)txtObs);
			((Control)panelControls).get_Controls().Add((Control)(object)textStar);
			((Control)panelControls).get_Controls().Add((Control)(object)panel2);
			((Control)panelControls).get_Controls().Add((Control)(object)cmdGmailHelp);
			((Control)panelControls).get_Controls().Add((Control)(object)lblFileName);
			((Control)panelControls).get_Controls().Add((Control)(object)cmdSpecifyDetails);
			((Control)panelControls).get_Controls().Add((Control)(object)cmdSaveSend);
			((Control)panelControls).get_Controls().Add((Control)(object)cmdSetEmails);
			((Control)panelControls).get_Controls().Add((Control)(object)grpRegion);
			((Control)panelControls).get_Controls().Add((Control)(object)cmdSetTimeScale);
			((Control)panelControls).get_Controls().Add((Control)(object)cmdIntegration);
			((Control)panelControls).get_Controls().Add((Control)(object)cmdIntegrity);
			((Control)panelControls).get_Controls().Add((Control)(object)panelComp);
			((Control)panelControls).get_Controls().Add((Control)(object)grpScale);
			((Control)panelControls).get_Controls().Add((Control)(object)panel1);
			((Control)panelControls).get_Controls().Add((Control)(object)label41);
			((Control)panelControls).get_Controls().Add((Control)(object)label43);
			((Control)panelControls).get_Controls().Add((Control)(object)label42);
			((Control)panelControls).set_Location(new Point(6, 353));
			((Control)panelControls).set_Name("panelControls");
			((Control)panelControls).set_Size(new Size(990, 219));
			((Control)panelControls).set_TabIndex(41);
			((Control)panel2).set_BackColor(Color.LightYellow);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdManageReportFiles);
			((Control)panel2).get_Controls().Add((Control)(object)cmdSave);
			((Control)panel2).get_Controls().Add((Control)(object)cmdEmailMultiple);
			((Control)panel2).set_Location(new Point(853, 57));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(126, 103));
			((Control)panel2).set_TabIndex(43);
			((Control)cmdGmailHelp).set_BackColor(Color.SkyBlue);
			((ButtonBase)cmdGmailHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdGmailHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdGmailHelp).set_Location(new Point(872, 191));
			((Control)cmdGmailHelp).set_Name("cmdGmailHelp");
			((Control)cmdGmailHelp).set_Size(new Size(88, 21));
			((Control)cmdGmailHelp).set_TabIndex(42);
			((Control)cmdGmailHelp).set_Text("GMail users");
			((ButtonBase)cmdGmailHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdGmailHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdGmailHelp).add_Click((EventHandler)cmdGmailHelp_Click);
			((Control)lblFileName).set_AutoSize(true);
			((Control)lblFileName).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFileName).set_ForeColor(Color.DarkRed);
			((Control)lblFileName).set_Location(new Point(818, 5));
			((Control)lblFileName).set_Name("lblFileName");
			((Control)lblFileName).set_Size(new Size(16, 13));
			((Control)lblFileName).set_TabIndex(41);
			((Control)lblFileName).set_Text("...");
			((Control)pnlShort).set_BackColor(Color.AntiqueWhite);
			pnlShort.set_BorderStyle((BorderStyle)2);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblCopied);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblShortEventWarning);
			((Control)pnlShort).get_Controls().Add((Control)(object)cmdHelp);
			((Control)pnlShort).get_Controls().Add((Control)(object)panel9);
			((Control)pnlShort).get_Controls().Add((Control)(object)picComp3);
			((Control)pnlShort).get_Controls().Add((Control)(object)picComp2);
			((Control)pnlShort).get_Controls().Add((Control)(object)picComp1);
			((Control)pnlShort).get_Controls().Add((Control)(object)picTarget);
			((Control)pnlShort).get_Controls().Add((Control)(object)panel8);
			((Control)pnlShort).get_Controls().Add((Control)(object)label47);
			((Control)pnlShort).get_Controls().Add((Control)(object)panel7);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblTarget);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblComp1);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblComp2);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblComp3);
			((Control)pnlShort).get_Controls().Add((Control)(object)label69);
			((Control)pnlShort).get_Controls().Add((Control)(object)lstComp3);
			((Control)pnlShort).get_Controls().Add((Control)(object)lstComp2);
			((Control)pnlShort).get_Controls().Add((Control)(object)lstComp1);
			((Control)pnlShort).get_Controls().Add((Control)(object)lstTarget);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblNoCompStars);
			((Control)pnlShort).get_Controls().Add((Control)(object)label72);
			((Control)pnlShort).get_Controls().Add((Control)(object)optComp2);
			((Control)pnlShort).get_Controls().Add((Control)(object)optComp1);
			((Control)pnlShort).get_Controls().Add((Control)(object)optComp3);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblHead2);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblHead3);
			((Control)pnlShort).get_Controls().Add((Control)(object)lblHead1);
			((Control)pnlShort).set_Location(new Point(6, 584));
			((Control)pnlShort).set_Name("pnlShort");
			((Control)pnlShort).set_Size(new Size(990, 185));
			((Control)pnlShort).set_TabIndex(42);
			((Control)lblCopied).set_AutoSize(true);
			((Control)lblCopied).set_BackColor(Color.Fuchsia);
			((Control)lblCopied).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCopied).set_ForeColor(Color.Yellow);
			((Control)lblCopied).set_Location(new Point(370, 129));
			((Control)lblCopied).set_Name("lblCopied");
			((Control)lblCopied).set_Size(new Size(101, 20));
			((Control)lblCopied).set_TabIndex(73);
			((Control)lblCopied).set_Text("Text copied");
			((Control)lblCopied).set_Visible(false);
			((Control)lblShortEventWarning).set_AutoSize(true);
			((Control)lblShortEventWarning).set_BackColor(Color.RoyalBlue);
			lblShortEventWarning.set_BorderStyle((BorderStyle)2);
			((Control)lblShortEventWarning).set_Font(new Font("Microsoft Sans Serif", 28f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblShortEventWarning).set_ForeColor(Color.Yellow);
			((Control)lblShortEventWarning).set_Location(new Point(304, 57));
			((Control)lblShortEventWarning).set_Name("lblShortEventWarning");
			((Control)lblShortEventWarning).set_Size(new Size(473, 46));
			((Control)lblShortEventWarning).set_TabIndex(70);
			((Control)lblShortEventWarning).set_Text("For 1, 2 && 3 frame events");
			((Control)lblShortEventWarning).set_Visible(false);
			((Control)cmdHelp).set_BackColor(Color.RoyalBlue);
			((ButtonBase)cmdHelp).set_Image((Image)Resources.help);
			((Control)cmdHelp).set_Location(new Point(1, 0));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(36, 36));
			((Control)cmdHelp).set_TabIndex(69);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)panel9).set_BackColor(Color.FromArgb(255, 255, 192));
			panel9.set_BorderStyle((BorderStyle)2);
			((Control)panel9).get_Controls().Add((Control)(object)lblFrame);
			((Control)panel9).get_Controls().Add((Control)(object)panel10);
			((Control)panel9).get_Controls().Add((Control)(object)lblRatio12);
			((Control)panel9).set_Location(new Point(11, 44));
			((Control)panel9).set_Name("panel9");
			((Control)panel9).set_Size(new Size(133, 45));
			((Control)panel9).set_TabIndex(68);
			((Control)lblFrame).set_AutoSize(true);
			((Control)lblFrame).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFrame).set_Location(new Point(79, 26));
			((Control)lblFrame).set_Name("lblFrame");
			((Control)lblFrame).set_Size(new Size(19, 13));
			((Control)lblFrame).set_TabIndex(31);
			((Control)lblFrame).set_Text("#0");
			((Control)panel10).set_BackColor(Color.Azure);
			((Control)panel10).get_Controls().Add((Control)(object)label10);
			((Control)panel10).get_Controls().Add((Control)(object)lblPossible);
			((Control)panel10).get_Controls().Add((Control)(object)label74);
			((Control)panel10).get_Controls().Add((Control)(object)lblProbable);
			((Control)panel10).get_Controls().Add((Control)(object)label73);
			((Control)panel10).set_Location(new Point(-1, -1));
			((Control)panel10).set_Name("panel10");
			((Control)panel10).set_Size(new Size(78, 47));
			((Control)panel10).set_TabIndex(28);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_ForeColor(Color.Purple);
			((Control)label10).set_Location(new Point(0, 1));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(73, 13));
			((Control)label10).set_TabIndex(71);
			((Control)label10).set_Text("Scaled values");
			((Control)lblPossible).set_AutoSize(true);
			((Control)lblPossible).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPossible).set_ForeColor(Color.Navy);
			((Control)lblPossible).set_Location(new Point(43, 28));
			((Control)lblPossible).set_Name("lblPossible");
			((Control)lblPossible).set_Size(new Size(34, 13));
			((Control)lblPossible).set_TabIndex(27);
			((Control)lblPossible).set_Text(">1.25");
			((Control)label74).set_AutoSize(true);
			((Control)label74).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label74).set_Location(new Point(-1, 28));
			((Control)label74).set_Name("label74");
			((Control)label74).set_Size(new Size(46, 13));
			((Control)label74).set_TabIndex(25);
			((Control)label74).set_Text("Possible");
			((Control)lblProbable).set_AutoSize(true);
			((Control)lblProbable).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblProbable).set_ForeColor(Color.FromArgb(0, 70, 0));
			((Control)lblProbable).set_Location(new Point(43, 15));
			((Control)lblProbable).set_Name("lblProbable");
			((Control)lblProbable).set_Size(new Size(34, 13));
			((Control)lblProbable).set_TabIndex(26);
			((Control)lblProbable).set_Text(">2.00");
			((Control)label73).set_AutoSize(true);
			((Control)label73).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label73).set_Location(new Point(-1, 15));
			((Control)label73).set_Name("label73");
			((Control)label73).set_Size(new Size(49, 13));
			((Control)label73).set_TabIndex(24);
			((Control)label73).set_Text("Probable");
			((Control)lblRatio12).set_AutoSize(true);
			((Control)lblRatio12).set_Font(new Font("Arial", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblRatio12).set_ForeColor(Color.FromArgb(210, 30, 0));
			((Control)lblRatio12).set_Location(new Point(77, 7));
			((Control)lblRatio12).set_Name("lblRatio12");
			((Control)lblRatio12).set_Size(new Size(32, 16));
			((Control)lblRatio12).set_TabIndex(21);
			((Control)lblRatio12).set_Text("1.00");
			picComp3.set_BorderStyle((BorderStyle)2);
			((Control)picComp3).set_Location(new Point(572, 34));
			((Control)picComp3).set_Name("picComp3");
			((Control)picComp3).set_Size(new Size(109, 55));
			picComp3.set_TabIndex(66);
			picComp3.set_TabStop(false);
			picComp2.set_BorderStyle((BorderStyle)2);
			((Control)picComp2).set_Location(new Point(431, 34));
			((Control)picComp2).set_Name("picComp2");
			((Control)picComp2).set_Size(new Size(109, 55));
			picComp2.set_TabIndex(65);
			picComp2.set_TabStop(false);
			picComp1.set_BorderStyle((BorderStyle)2);
			((Control)picComp1).set_Location(new Point(290, 34));
			((Control)picComp1).set_Name("picComp1");
			((Control)picComp1).set_Size(new Size(109, 55));
			picComp1.set_TabIndex(64);
			picComp1.set_TabStop(false);
			picTarget.set_BorderStyle((BorderStyle)2);
			((Control)picTarget).set_Location(new Point(152, 34));
			((Control)picTarget).set_Name("picTarget");
			((Control)picTarget).set_Size(new Size(109, 55));
			picTarget.set_TabIndex(63);
			picTarget.set_TabStop(false);
			panel8.set_BorderStyle((BorderStyle)2);
			((Control)panel8).get_Controls().Add((Control)(object)label75);
			((Control)panel8).get_Controls().Add((Control)(object)label71);
			((Control)panel8).get_Controls().Add((Control)(object)label70);
			((Control)panel8).get_Controls().Add((Control)(object)label68);
			((Control)panel8).get_Controls().Add((Control)(object)label67);
			((Control)panel8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panel8).set_Location(new Point(781, 9));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(178, 164));
			((Control)panel8).set_TabIndex(62);
			((Control)label75).set_AutoSize(true);
			((Control)label75).set_ForeColor(Color.MediumBlue);
			((Control)label75).set_Location(new Point(0, 66));
			((Control)label75).set_Name("label75");
			((Control)label75).set_Size(new Size(170, 26));
			((Control)label75).set_TabIndex(9);
			((Control)label75).set_Text("Value varies according to the\r\n# data points && # comparison stars");
			((Control)label71).set_AutoSize(true);
			((Control)label71).set_ForeColor(Color.DarkRed);
			((Control)label71).set_Location(new Point(0, 132));
			((Control)label71).set_Name("label71");
			((Control)label71).set_Size(new Size(155, 26));
			((Control)label71).set_TabIndex(8);
			((Control)label71).set_Text("Comparisons in light red may be\r\nunsuitable");
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_ForeColor(Color.FromArgb(0, 70, 0));
			((Control)label70).set_Location(new Point(0, 93));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(162, 39));
			((Control)label70).set_TabIndex(7);
			((Control)label70).set_Text("Entries in target after the first are \r\nconsistent with the entries in all\r\nthe comparisons");
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label68).set_Location(new Point(31, -2));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(103, 17));
			((Control)label68).set_TabIndex(6);
			((Control)label68).set_Text("Basic criteria");
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_ForeColor(Color.Teal);
			((Control)label67).set_Location(new Point(0, 13));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(164, 52));
			((Control)label67).set_TabIndex(5);
			((Control)label67).set_Text("First entry in Target at least about\r\n30% greater than:\r\n*  2nd  entry in target, and\r\n*  all entries in all Comparisons");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(710, 6));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(54, 26));
			((Control)label47).set_TabIndex(61);
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
			((Control)panel7).set_Location(new Point(713, 34));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(48, 143));
			((Control)panel7).set_TabIndex(60);
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
			((Control)opt57).set_Location(new Point(3, 120));
			((Control)opt57).set_Name("opt57");
			((Control)opt57).set_Size(new Size(39, 17));
			((Control)opt57).set_TabIndex(6);
			((Control)opt57).set_Text("57");
			((ButtonBase)opt57).set_UseVisualStyleBackColor(true);
			((Control)opt57).add_Click((EventHandler)opt57_Click);
			opt41.set_AutoCheck(false);
			((Control)opt41).set_AutoSize(true);
			((Control)opt41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt41).set_Location(new Point(3, 80));
			((Control)opt41).set_Name("opt41");
			((Control)opt41).set_Size(new Size(39, 17));
			((Control)opt41).set_TabIndex(4);
			((Control)opt41).set_Text("41");
			((ButtonBase)opt41).set_UseVisualStyleBackColor(true);
			((Control)opt41).add_Click((EventHandler)opt41_Click);
			opt49.set_AutoCheck(false);
			((Control)opt49).set_AutoSize(true);
			((Control)opt49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt49).set_Location(new Point(3, 100));
			((Control)opt49).set_Name("opt49");
			((Control)opt49).set_Size(new Size(39, 17));
			((Control)opt49).set_TabIndex(5);
			((Control)opt49).set_Text("49");
			((ButtonBase)opt49).set_UseVisualStyleBackColor(true);
			((Control)opt49).add_Click((EventHandler)opt49_Click);
			opt33.set_AutoCheck(false);
			((Control)opt33).set_AutoSize(true);
			((Control)opt33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)opt33).set_Location(new Point(3, 60));
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
			((Control)opt25).set_Location(new Point(3, 40));
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
			((Control)opt17).set_Location(new Point(3, 20));
			((Control)opt17).set_Name("opt17");
			((Control)opt17).set_Size(new Size(39, 17));
			((Control)opt17).set_TabIndex(1);
			((Control)opt17).set_Text("17");
			((ButtonBase)opt17).set_UseVisualStyleBackColor(true);
			((Control)opt17).add_Click((EventHandler)opt17_Click);
			((Control)lblTarget).set_AutoSize(true);
			((Control)lblTarget).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTarget).set_ForeColor(Color.FromArgb(0, 70, 0));
			((Control)lblTarget).set_Location(new Point(144, 20));
			((Control)lblTarget).set_Name("lblTarget");
			((Control)lblTarget).set_Size(new Size(125, 13));
			((Control)lblTarget).set_TabIndex(58);
			((Control)lblTarget).set_Text("Target  brightness = 1.00");
			((Control)lblComp1).set_AutoSize(true);
			((Control)lblComp1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblComp1).set_ForeColor(Color.Blue);
			((Control)lblComp1).set_Location(new Point(287, 20));
			((Control)lblComp1).set_Name("lblComp1");
			((Control)lblComp1).set_Size(new Size(76, 13));
			((Control)lblComp1).set_TabIndex(57);
			((Control)lblComp1).set_Text("Comp 1 = 1.00");
			((Control)lblComp2).set_AutoSize(true);
			((Control)lblComp2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblComp2).set_ForeColor(Color.Blue);
			((Control)lblComp2).set_Location(new Point(429, 20));
			((Control)lblComp2).set_Name("lblComp2");
			((Control)lblComp2).set_Size(new Size(76, 13));
			((Control)lblComp2).set_TabIndex(56);
			((Control)lblComp2).set_Text("Comp 2 = 1.00");
			((Control)lblComp3).set_AutoSize(true);
			((Control)lblComp3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblComp3).set_ForeColor(Color.Blue);
			((Control)lblComp3).set_Location(new Point(570, 20));
			((Control)lblComp3).set_Name("lblComp3");
			((Control)lblComp3).set_Size(new Size(76, 13));
			((Control)lblComp3).set_TabIndex(55);
			((Control)lblComp3).set_Text("Comp 3 = 1.00");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label69).set_Location(new Point(45, 6));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(182, 13));
			((Control)label69).set_TabIndex(54);
			((Control)label69).set_Text("Target  (with comparison stars)");
			((Control)lblNoCompStars).set_AutoSize(true);
			((Control)lblNoCompStars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblNoCompStars).set_ForeColor(Color.Red);
			((Control)lblNoCompStars).set_Location(new Point(274, 97));
			((Control)lblNoCompStars).set_Name("lblNoCompStars");
			((Control)lblNoCompStars).set_Size(new Size(393, 39));
			((Control)lblNoCompStars).set_TabIndex(59);
			((Control)lblNoCompStars).set_Text("The light curve data does not contain data for any comparison star.\r\nThis prevents a comparison with the noise of other stars.\r\nAs a result, any conclusion derived from the Target list is unreliable.");
			((Control)lblNoCompStars).set_Visible(false);
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label72).set_ForeColor(Color.FromArgb(210, 30, 0));
			((Control)label72).set_Location(new Point(64, 25));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(84, 17));
			((Control)label72).set_TabIndex(67);
			((Control)label72).set_Text("Evaluation");
			optComp2.set_AutoCheck(false);
			((Control)optComp2).set_AutoSize(true);
			((Control)optComp2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optComp2).set_Location(new Point(508, 18));
			((Control)optComp2).set_Name("optComp2");
			((Control)optComp2).set_Size(new Size(43, 17));
			((Control)optComp2).set_TabIndex(75);
			optComp2.set_TabStop(true);
			((Control)optComp2).set_Text("Use");
			((ButtonBase)optComp2).set_UseVisualStyleBackColor(true);
			((Control)optComp2).add_Click((EventHandler)optComp2_Click);
			optComp1.set_AutoCheck(false);
			((Control)optComp1).set_AutoSize(true);
			((Control)optComp1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optComp1).set_Location(new Point(367, 18));
			((Control)optComp1).set_Name("optComp1");
			((Control)optComp1).set_Size(new Size(43, 17));
			((Control)optComp1).set_TabIndex(74);
			optComp1.set_TabStop(true);
			((Control)optComp1).set_Text("Use");
			((ButtonBase)optComp1).set_UseVisualStyleBackColor(true);
			((Control)optComp1).add_Click((EventHandler)optComp1_Click);
			optComp3.set_AutoCheck(false);
			((Control)optComp3).set_AutoSize(true);
			((Control)optComp3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optComp3).set_Location(new Point(649, 18));
			((Control)optComp3).set_Name("optComp3");
			((Control)optComp3).set_Size(new Size(43, 17));
			((Control)optComp3).set_TabIndex(76);
			optComp3.set_TabStop(true);
			((Control)optComp3).set_Text("Use");
			((ButtonBase)optComp3).set_UseVisualStyleBackColor(true);
			((Control)optComp3).add_Click((EventHandler)optComp3_Click);
			((Control)lblHead2).set_AutoSize(true);
			((Control)lblHead2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHead2).set_Location(new Point(440, 4));
			((Control)lblHead2).set_Name("lblHead2");
			((Control)lblHead2).set_Size(new Size(91, 13));
			((Control)lblHead2).set_TabIndex(53);
			((Control)lblHead2).set_Text("Comparison #2");
			((Control)lblHead3).set_AutoSize(true);
			((Control)lblHead3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHead3).set_Location(new Point(581, 4));
			((Control)lblHead3).set_Name("lblHead3");
			((Control)lblHead3).set_Size(new Size(91, 13));
			((Control)lblHead3).set_TabIndex(52);
			((Control)lblHead3).set_Text("Comparison #3");
			((Control)lblHead1).set_AutoSize(true);
			((Control)lblHead1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHead1).set_Location(new Point(299, 4));
			((Control)lblHead1).set_Name("lblHead1");
			((Control)lblHead1).set_Size(new Size(91, 13));
			((Control)lblHead1).set_TabIndex(51);
			((Control)lblHead1).set_Text("Comparison #1");
			((Control)cmdManageReportFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdManageReportFiles).set_Location(new Point(1, 63));
			((Control)cmdManageReportFiles).set_Name("cmdManageReportFiles");
			((Control)cmdManageReportFiles).set_Size(new Size(120, 34));
			((Control)cmdManageReportFiles).set_TabIndex(17);
			((Control)cmdManageReportFiles).set_Text("Manage report files");
			((ButtonBase)cmdManageReportFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdManageReportFiles).add_Click((EventHandler)cmdManageReportFiles_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1026, 772));
			((Control)this).get_Controls().Add((Control)(object)pnlShort);
			((Control)this).get_Controls().Add((Control)(object)panelControls);
			((Control)this).get_Controls().Add((Control)(object)panelPlot);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("LightCurve");
			((Control)this).set_Text("Report a Light Curve");
			((Form)this).add_FormClosing(new FormClosingEventHandler(LightCurve_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(LightCurve_FormClosed));
			((Form)this).add_Load((EventHandler)LightCurve_Load);
			((Control)this).add_Resize((EventHandler)LightCurve_Resize);
			((ISupportInitialize)updnScale).EndInit();
			((Control)panelPlot).ResumeLayout(false);
			((ISupportInitialize)picPlot).EndInit();
			((Control)panelEmailAddresses).ResumeLayout(false);
			((Control)panelEmailAddresses).PerformLayout();
			((ISupportInitialize)pictureBox1).EndInit();
			((Control)grpScale).ResumeLayout(false);
			((Control)grpScale).PerformLayout();
			((ISupportInitialize)updnVerticalScaleAdjustment).EndInit();
			((Control)panelComp).ResumeLayout(false);
			((Control)panelComp).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpRegion).ResumeLayout(false);
			((Control)grpRegion).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panelControls).ResumeLayout(false);
			((Control)panelControls).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)pnlShort).ResumeLayout(false);
			((Control)pnlShort).PerformLayout();
			((Control)panel9).ResumeLayout(false);
			((Control)panel9).PerformLayout();
			((Control)panel10).ResumeLayout(false);
			((Control)panel10).PerformLayout();
			((ISupportInitialize)picComp3).EndInit();
			((ISupportInitialize)picComp2).EndInit();
			((ISupportInitialize)picComp1).EndInit();
			((ISupportInitialize)picTarget).EndInit();
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
