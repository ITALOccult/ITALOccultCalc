using System;
using System.ComponentModel;
using System.Configuration;
using System.Drawing;
using System.Windows.Forms;
using Occult.Defaults___Settings;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class SelectAndDisplay : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private static double PlotMagnify;

		private static double LastPlotMagnify;

		private IContainer components;

		private CheckBox chkSaveElements;

		private Button cmdPlotMultiPaths;

		private Button cmdPlanetEventTimes;

		private Button cmdPlotEvents;

		private Button cmdGlobalSummaryOfEvents;

		private Button cmdSummaryOfEvents;

		private GroupBox groupBox5;

		private RadioButton opt001Day;

		private RadioButton opt01Day;

		private RadioButton opt1Day;

		private Button cmdDWDfile;

		private GroupBox grpSelectionCriteria;

		private CheckBox chkBinary;

		private Button cmdReSetSites;

		private NumericUpDown updnScale;

		private Label label6;

		private ComboBox cmbDay2;

		private ComboBox cmbDay1;

		private ComboBox cmbMonth2;

		private ComboBox cmbMonth1;

		private NumericUpDown updnYear2;

		private NumericUpDown updnYear1;

		private ComboBox cmbPlanet;

		private Label label5;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private TextBox txtAsteroidName;

		private TextBox txtAsteroidNumber;

		private TextBox txtDia;

		private TextBox txtDuration;

		private TextBox txtMagDrop;

		private TextBox txtDistance;

		private TextBox txtKM;

		private TextBox txtLatitude;

		private TextBox txtLongitude;

		private CheckBox chkMagDrop;

		private CheckBox chkDuration;

		private CheckBox chkDiameter;

		private CheckBox chkAsteroidNumber;

		private CheckBox chkAsteroidName;

		private CheckBox chkDate;

		private CheckBox chkPlanet;

		private CheckBox chkTransNeptune;

		private CheckBox chkDistArcSec;

		private CheckBox chkSiteDistKM;

		private CheckBox chkSite;

		private CheckBox chkAutoGenerate;

		private Label label8;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private GroupBox groupBox3;

		private Label label9;

		private Label label7;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label10;

		private Label label11;

		private Label label12;

		private CheckBox chkSolarElongation;

		private TextBox txtSolarElong;

		private CheckBox chkShapeModel;

		private CheckBox chkLocalAltitude;

		private Label label13;

		private NumericUpDown updnLocalAltitude;

		public SelectAndDisplay()
		{
			InitializeComponent();
		}

		private void SelectAndDisplay_Load(object sender, EventArgs e)
		{
			//IL_0097: Unknown result type (might be due to invalid IL or missing references)
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			DateTime now = DateTime.Now;
			updnYear1.set_Value((decimal)now.ToUniversalTime().Year);
			((ListControl)cmbMonth1).set_SelectedIndex(now.ToUniversalTime().Month - 1);
			((ListControl)cmbDay1).set_SelectedIndex(now.ToUniversalTime().Day - 1);
			((ListControl)cmbPlanet).set_SelectedIndex(0);
			DisplayMPOccultations.LoadISAM_and_DAMIT_ids(UpdateISAM: true);
			MessageBox.Show("You have reached this form by selecting the \r\n'Read and Display' button on the main form. \r\n\r\nOccult has a new approach to displaying \r\nasteroidal occultation events, under the button \r\n'List and Display occultations'.\r\n\r\nIt is my intention to remove the 'Read and Display' \r\napproach from Occult in early 2018.\r\n\r\nIf you want me to retain the 'Read and Display' approach,\r\nplease advise me as soon as possible, setting out\r\n your reasons for wanting the functionality retained.\r\n\r\nDave Herald\r\ndrherald@bigpond.net.au", "Discontinued approach", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void cmdPlotEvents_Click(object sender, EventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.FileInUse)
			{
				MessageBox.Show("You cannot start a new Plot of events while you are Plotting events from a file.\r\n\r\nSwitch to the form displaying an occultation, and click the menu item 'Exit'", "Error - File in use", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			SetSearchCriteriaVariables(Global: false);
			DisplayMPOccultations.InitialisePlot();
		}

		private void cmdGlobalSummaryOfEvents_Click(object sender, EventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.FileInUse)
			{
				MessageBox.Show("You cannot create a summary list while you are Plotting events from a file.\r\n\r\nSwitch to the form displaying an occultation, and click the menu item 'Exit'", "Error - File in use", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			SetSearchCriteriaVariables(Global: true);
			DisplayMPOccultations.CreateSummaryOfEvents(Global: true, PlanetTimes: false, SaveElements: false);
		}

		private void cmdSummaryOfEvents_Click(object sender, EventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.FileInUse)
			{
				MessageBox.Show("You cannot create a summary list while you are displaying results from a file.\r\n\r\nSwitch to the form displaying an occultation, and click the menu item 'Exit'", "Error - File in use", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			SetSearchCriteriaVariables(Global: false);
			DisplayMPOccultations.CreateSummaryOfEvents(Global: false, PlanetTimes: false, chkSaveElements.get_Checked());
		}

		private void cmdPlotMultiPaths_Click(object sender, EventArgs e)
		{
			double.TryParse(((Control)txtLongitude).get_Text(), out DisplayMPOccultations.SiteLongitude_Rad);
			DisplayMPOccultations.SiteLongitude_Rad /= 180.0 / Math.PI;
			double.TryParse(((Control)txtLatitude).get_Text(), out DisplayMPOccultations.SiteLatitude_Rad);
			DisplayMPOccultations.SiteLatitude_Rad /= 180.0 / Math.PI;
			DisplayMPOccultations.Plot_MultiPaths(FromListAndDisplay: false);
		}

		private void cmdPlanetEventTimes_Click(object sender, EventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.FileInUse)
			{
				MessageBox.Show("You cannot create a Planet contact time list while you are Plotting events from a file.\r\n\r\nSwitch to the form displaying an occultation, and click the menu item 'Exit'", "Error - File in use", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			SetSearchCriteriaVariables(Global: true);
			DisplayMPOccultations.CreateSummaryOfEvents(Global: true, PlanetTimes: true, SaveElements: false);
		}

		private void cmdDWDfile_Click(object sender, EventArgs e)
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			if (DisplayMPOccultations.FileInUse)
			{
				MessageBox.Show("You cannot create a DWD file while you are Plotting events from a file.\r\n\r\nSwitch to the form displaying an occultation, and click the menu item 'Exit'", "Error - File in use", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				if (!DisplayMPOccultations.OpenOccultationElementFile(DWD: true, ""))
				{
					return;
				}
				for (int i = 0; i < DisplayMPOccultations.RecordsInFile; i++)
				{
					bool MeetsCriteria = false;
					double interval = 1.0;
					if (opt01Day.get_Checked())
					{
						interval = 0.1;
					}
					if (opt001Day.get_Checked())
					{
						interval = 0.01;
					}
					SetSearchCriteriaVariables(Global: false);
					DisplayMPOccultations.Get_Data_From_540Record(i, out MeetsCriteria, GeneratingSummary: false, AutoPlot: false);
					if (MeetsCriteria)
					{
						DisplayMPOccultations.MakeDWD_OutputBlock(interval);
					}
				}
				DisplayMPOccultations.DWDFile.Close();
				DisplayMPOccultations.Elements.Close();
			}
		}

		private void SetSearchCriteriaVariables(bool Global)
		{
			DisplayMPOccultations.FilterOnSite = chkSite.get_Checked();
			if (Global)
			{
				DisplayMPOccultations.FilterOnSite = false;
			}
			DisplayMPOccultations.FilterOnKM = chkSiteDistKM.get_Checked();
			DisplayMPOccultations.FilterOnArcSec = chkDistArcSec.get_Checked();
			DisplayMPOccultations.FilterOnMagDrop = chkMagDrop.get_Checked();
			DisplayMPOccultations.FilterOnDuration = chkDuration.get_Checked();
			DisplayMPOccultations.FilterOnDiameter = chkDiameter.get_Checked();
			DisplayMPOccultations.FilterOnTransNeptune = chkTransNeptune.get_Checked();
			DisplayMPOccultations.FilterOnNumber = chkAsteroidNumber.get_Checked();
			DisplayMPOccultations.FilterOnName = chkAsteroidName.get_Checked();
			DisplayMPOccultations.FilterOnDate = chkDate.get_Checked();
			DisplayMPOccultations.FilterOnPlanet = chkPlanet.get_Checked();
			DisplayMPOccultations.FilterOnTransNeptune = chkTransNeptune.get_Checked();
			DisplayMPOccultations.FilterOnBinary = chkBinary.get_Checked();
			DisplayMPOccultations.FilterOnSolarElongation = chkSolarElongation.get_Checked();
			DisplayMPOccultations.FilterOnShapeModel = chkShapeModel.get_Checked();
			DisplayMPOccultations.FilterOnLocalAltitude = chkLocalAltitude.get_Checked();
			DisplayMPOccultations.FilterDuration = (DisplayMPOccultations.FilterMagDrop = (DisplayMPOccultations.FilterDiameter = (DisplayMPOccultations.SiteLongitude_Rad = (DisplayMPOccultations.SiteLatitude_Rad = 0.0))));
			double.TryParse(((Control)txtLongitude).get_Text(), out DisplayMPOccultations.SiteLongitude_Rad);
			DisplayMPOccultations.SiteLongitude_Rad /= 180.0 / Math.PI;
			DisplayMPOccultations.SiteLongitudeText = ((Control)txtLongitude).get_Text().Trim();
			double.TryParse(((Control)txtLatitude).get_Text(), out DisplayMPOccultations.SiteLatitude_Rad);
			DisplayMPOccultations.SiteLatitude_Rad /= 180.0 / Math.PI;
			DisplayMPOccultations.SiteLatitudeText = ((Control)txtLatitude).get_Text().Trim();
			double.TryParse(((Control)txtKM).get_Text(), out DisplayMPOccultations.FilterKM);
			double.TryParse(((Control)txtDistance).get_Text(), out DisplayMPOccultations.FilterArcSec);
			double.TryParse(((Control)txtMagDrop).get_Text(), out DisplayMPOccultations.FilterMagDrop);
			double.TryParse(((Control)txtDuration).get_Text(), out DisplayMPOccultations.FilterDuration);
			double.TryParse(((Control)txtDia).get_Text(), out DisplayMPOccultations.FilterDiameter);
			double.TryParse(((Control)txtSolarElong).get_Text(), out DisplayMPOccultations.FilterSolarElongation);
			int.TryParse(((Control)txtAsteroidNumber).get_Text(), out DisplayMPOccultations.FilterAsteroidNumber);
			DisplayMPOccultations.FilterLocalAltitude = Math.Sin((double)updnLocalAltitude.get_Value() / (180.0 / Math.PI));
			DisplayMPOccultations.FilterName = ((Control)txtAsteroidName).get_Text().Trim();
			DisplayMPOccultations.FirstFilterDate = Utilities.JD_from_Date((int)updnYear1.get_Value(), ((ListControl)cmbMonth1).get_SelectedIndex() + 1, ((ListControl)cmbDay1).get_SelectedIndex() + 1);
			DisplayMPOccultations.LastFilterDate = Utilities.JD_from_Date((int)updnYear2.get_Value(), ((ListControl)cmbMonth2).get_SelectedIndex() + 1, ((ListControl)cmbDay2).get_SelectedIndex() + 1);
			DisplayMPOccultations.FilterPlanetNumber = ((ListControl)cmbPlanet).get_SelectedIndex() + 1;
			if (DisplayMPOccultations.FilterPlanetNumber > 2)
			{
				DisplayMPOccultations.FilterPlanetNumber++;
			}
			DisplayMPOccultations.PlotMagnification_Initial = (double)updnScale.get_Value();
		}

		private void cmdReSetSites_Click(object sender, EventArgs e)
		{
			//IL_0007: Unknown result type (might be due to invalid IL or missing references)
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0014: Invalid comparison between Unknown and I4
			SiteSelector siteSelector = new SiteSelector();
			((Form)siteSelector).ShowDialog();
			if ((int)((Form)siteSelector).get_DialogResult() == 1)
			{
				((Control)txtLongitude).set_Text(((Control)siteSelector.txtLongitude).get_Text());
				((Control)txtLatitude).set_Text(((Control)siteSelector.txtLatitude).get_Text());
				Settings.Default.AsteroidSiteLongitude = ((Control)txtLongitude).get_Text();
				Settings.Default.AsteroidSiteLatitude = ((Control)txtLatitude).get_Text();
			}
			((Component)(object)siteSelector).Dispose();
		}

		private void chkSite_CheckedChanged(object sender, EventArgs e)
		{
		}

		private void updnScale_Click(object sender, EventArgs e)
		{
			PlotMagnify = (double)updnScale.get_Value();
			if (PlotMagnify > LastPlotMagnify)
			{
				if ((PlotMagnify > 2.0) & (PlotMagnify < 4.0))
				{
					PlotMagnify = 4.0;
				}
				if ((PlotMagnify > 4.0) & (PlotMagnify < 8.0))
				{
					PlotMagnify = 8.0;
				}
				if ((PlotMagnify > 8.0) & (PlotMagnify < 16.0))
				{
					PlotMagnify = 16.0;
				}
			}
			else if (PlotMagnify < LastPlotMagnify)
			{
				if ((PlotMagnify > 2.0) & (PlotMagnify < 4.0))
				{
					PlotMagnify = 2.0;
				}
				if ((PlotMagnify > 4.0) & (PlotMagnify < 8.0))
				{
					PlotMagnify = 4.0;
				}
				if ((PlotMagnify > 8.0) & (PlotMagnify < 16.0))
				{
					PlotMagnify = 8.0;
				}
			}
			LastPlotMagnify = PlotMagnify;
			updnScale.set_Value((decimal)PlotMagnify);
		}

		private void updnScale_DoubleClick(object sender, EventArgs e)
		{
			updnScale_Click(sender, e);
		}

		private void updnYear1_ValueChanged(object sender, EventArgs e)
		{
			SetDefaultEndDateForSearchCriteria();
		}

		private void cmbMonth1_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetDefaultEndDateForSearchCriteria();
		}

		private void cmbDay1_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetDefaultEndDateForSearchCriteria();
		}

		private void SetDefaultEndDateForSearchCriteria()
		{
			int Year = (int)updnYear1.get_Value();
			int Month = ((ListControl)cmbMonth1).get_SelectedIndex() + 1;
			double day = (double)((ListControl)cmbDay1).get_SelectedIndex() + 1.0;
			Utilities.Date_from_JD(Utilities.JD_from_Date(Year, Month, day) + (double)DisplayMPOccultations.AsteroidDateRange, out Year, out Month, out day);
			updnYear2.set_Value((decimal)Year);
			((ListControl)cmbMonth2).set_SelectedIndex(Month - 1);
			((ListControl)cmbDay2).set_SelectedIndex((int)(day - 1.0));
		}

		private void updnYear1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear1).Select(0, 10);
		}

		private void updnYear2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear2).Select(0, 10);
		}

		private void txtLongitude_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongitude).SelectAll();
		}

		private void txtLatitude_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatitude).SelectAll();
		}

		private void txtKM_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtKM).SelectAll();
		}

		private void txtDistance_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDistance).SelectAll();
		}

		private void txtMagDrop_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMagDrop).SelectAll();
		}

		private void txtDuration_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDuration).SelectAll();
		}

		private void txtDia_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDia).SelectAll();
		}

		private void txtAsteroidNumber_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAsteroidNumber).SelectAll();
		}

		private void txtAsteroidName_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAsteroidName).SelectAll();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((SettingsBase)Settings.Default).Save();
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Display Asteroids");
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
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
			//IL_102d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1037: Expected O, but got Unknown
			//IL_1126: Unknown result type (might be due to invalid IL or missing references)
			//IL_1130: Expected O, but got Unknown
			//IL_12e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_12f1: Expected O, but got Unknown
			//IL_13ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_13b5: Expected O, but got Unknown
			//IL_2271: Unknown result type (might be due to invalid IL or missing references)
			//IL_227b: Expected O, but got Unknown
			//IL_232b: Unknown result type (might be due to invalid IL or missing references)
			//IL_2335: Expected O, but got Unknown
			//IL_23e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_23ef: Expected O, but got Unknown
			//IL_249f: Unknown result type (might be due to invalid IL or missing references)
			//IL_24a9: Expected O, but got Unknown
			//IL_2556: Unknown result type (might be due to invalid IL or missing references)
			//IL_2560: Expected O, but got Unknown
			//IL_274d: Unknown result type (might be due to invalid IL or missing references)
			//IL_2757: Expected O, but got Unknown
			//IL_2818: Unknown result type (might be due to invalid IL or missing references)
			//IL_2822: Expected O, but got Unknown
			//IL_28e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_28ed: Expected O, but got Unknown
			//IL_2b6d: Unknown result type (might be due to invalid IL or missing references)
			//IL_2b77: Expected O, but got Unknown
			//IL_2c38: Unknown result type (might be due to invalid IL or missing references)
			//IL_2c42: Expected O, but got Unknown
			//IL_2d03: Unknown result type (might be due to invalid IL or missing references)
			//IL_2d0d: Expected O, but got Unknown
			//IL_2dd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_2de1: Expected O, but got Unknown
			//IL_2eaa: Unknown result type (might be due to invalid IL or missing references)
			//IL_2eb4: Expected O, but got Unknown
			//IL_3083: Unknown result type (might be due to invalid IL or missing references)
			//IL_308d: Expected O, but got Unknown
			//IL_32b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_32c3: Expected O, but got Unknown
			chkSaveElements = new CheckBox();
			cmdPlotMultiPaths = new Button();
			cmdPlanetEventTimes = new Button();
			cmdPlotEvents = new Button();
			cmdGlobalSummaryOfEvents = new Button();
			cmdSummaryOfEvents = new Button();
			groupBox5 = new GroupBox();
			label9 = new Label();
			label7 = new Label();
			opt001Day = new RadioButton();
			opt01Day = new RadioButton();
			opt1Day = new RadioButton();
			cmdDWDfile = new Button();
			grpSelectionCriteria = new GroupBox();
			label13 = new Label();
			updnLocalAltitude = new NumericUpDown();
			chkLocalAltitude = new CheckBox();
			chkShapeModel = new CheckBox();
			label12 = new Label();
			txtSolarElong = new TextBox();
			chkSolarElongation = new CheckBox();
			label11 = new Label();
			label10 = new Label();
			chkBinary = new CheckBox();
			cmdReSetSites = new Button();
			label6 = new Label();
			cmbDay2 = new ComboBox();
			cmbDay1 = new ComboBox();
			cmbMonth2 = new ComboBox();
			cmbMonth1 = new ComboBox();
			updnYear2 = new NumericUpDown();
			updnYear1 = new NumericUpDown();
			cmbPlanet = new ComboBox();
			label5 = new Label();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtAsteroidName = new TextBox();
			txtAsteroidNumber = new TextBox();
			txtDia = new TextBox();
			txtDuration = new TextBox();
			txtMagDrop = new TextBox();
			txtDistance = new TextBox();
			txtKM = new TextBox();
			txtLatitude = new TextBox();
			txtLongitude = new TextBox();
			chkMagDrop = new CheckBox();
			chkDuration = new CheckBox();
			chkDiameter = new CheckBox();
			chkAsteroidNumber = new CheckBox();
			chkAsteroidName = new CheckBox();
			chkDate = new CheckBox();
			chkPlanet = new CheckBox();
			chkTransNeptune = new CheckBox();
			chkDistArcSec = new CheckBox();
			chkSiteDistKM = new CheckBox();
			chkSite = new CheckBox();
			label8 = new Label();
			groupBox1 = new GroupBox();
			updnScale = new NumericUpDown();
			groupBox2 = new GroupBox();
			chkAutoGenerate = new CheckBox();
			groupBox3 = new GroupBox();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			((Control)groupBox5).SuspendLayout();
			((Control)grpSelectionCriteria).SuspendLayout();
			((ISupportInitialize)updnLocalAltitude).BeginInit();
			((ISupportInitialize)updnYear2).BeginInit();
			((ISupportInitialize)updnYear1).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnScale).BeginInit();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)chkSaveElements).set_AutoSize(true);
			chkSaveElements.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkSaveElements).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSaveElements).set_Location(new Point(175, 63));
			((Control)chkSaveElements).set_Name("chkSaveElements");
			((Control)chkSaveElements).set_Size(new Size(167, 30));
			((Control)chkSaveElements).set_TabIndex(2);
			((Control)chkSaveElements).set_Text(" Local events summary - Save\r\nelements for OccultWatcher");
			((ButtonBase)chkSaveElements).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkSaveElements).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotMultiPaths).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPlotMultiPaths).set_Location(new Point(164, 104));
			((Control)cmdPlotMultiPaths).set_Name("cmdPlotMultiPaths");
			((Control)cmdPlotMultiPaths).set_Size(new Size(178, 37));
			((Control)cmdPlotMultiPaths).set_TabIndex(4);
			((Control)cmdPlotMultiPaths).set_Text("Draw paths from the last Local\r\n or Global events summary");
			((ButtonBase)cmdPlotMultiPaths).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotMultiPaths).add_Click((EventHandler)cmdPlotMultiPaths_Click);
			((Control)cmdPlanetEventTimes).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlanetEventTimes).set_Location(new Point(18, 185));
			((Control)cmdPlanetEventTimes).set_Name("cmdPlanetEventTimes");
			((Control)cmdPlanetEventTimes).set_Size(new Size(124, 47));
			((Control)cmdPlanetEventTimes).set_TabIndex(5);
			((Control)cmdPlanetEventTimes).set_Text("Planets: local\r\ntimes summary");
			((ButtonBase)cmdPlanetEventTimes).set_UseVisualStyleBackColor(true);
			((Control)cmdPlanetEventTimes).add_Click((EventHandler)cmdPlanetEventTimes_Click);
			((Control)cmdPlotEvents).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlotEvents).set_Location(new Point(18, 38));
			((Control)cmdPlotEvents).set_Name("cmdPlotEvents");
			((Control)cmdPlotEvents).set_Size(new Size(124, 47));
			((Control)cmdPlotEvents).set_TabIndex(0);
			((Control)cmdPlotEvents).set_Text("Plot events");
			((ButtonBase)cmdPlotEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotEvents).add_Click((EventHandler)cmdPlotEvents_Click);
			((Control)cmdGlobalSummaryOfEvents).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGlobalSummaryOfEvents).set_Location(new Point(18, 122));
			((Control)cmdGlobalSummaryOfEvents).set_Name("cmdGlobalSummaryOfEvents");
			((Control)cmdGlobalSummaryOfEvents).set_Size(new Size(124, 47));
			((Control)cmdGlobalSummaryOfEvents).set_TabIndex(3);
			((Control)cmdGlobalSummaryOfEvents).set_Text("Global events:\r\nSummary");
			((ButtonBase)cmdGlobalSummaryOfEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdGlobalSummaryOfEvents).add_Click((EventHandler)cmdGlobalSummaryOfEvents_Click);
			((Control)cmdSummaryOfEvents).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSummaryOfEvents).set_Location(new Point(18, 59));
			((Control)cmdSummaryOfEvents).set_Name("cmdSummaryOfEvents");
			((Control)cmdSummaryOfEvents).set_Size(new Size(124, 47));
			((Control)cmdSummaryOfEvents).set_TabIndex(1);
			((Control)cmdSummaryOfEvents).set_Text("Local events:\r\nSummary");
			((ButtonBase)cmdSummaryOfEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdSummaryOfEvents).add_Click((EventHandler)cmdSummaryOfEvents_Click);
			((Control)groupBox5).get_Controls().Add((Control)(object)label9);
			((Control)groupBox5).get_Controls().Add((Control)(object)label7);
			((Control)groupBox5).get_Controls().Add((Control)(object)opt001Day);
			((Control)groupBox5).get_Controls().Add((Control)(object)opt01Day);
			((Control)groupBox5).get_Controls().Add((Control)(object)opt1Day);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdDWDfile);
			((Control)groupBox5).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)groupBox5).set_Location(new Point(517, 481));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(353, 106));
			((Control)groupBox5).set_TabIndex(5);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Create DWD file");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(9, 22));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(366, 12));
			((Control)label9).set_TabIndex(0);
			((Control)label9).set_Text("This functionality is not for general use. It provides support for a legacy prediction system");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(10, 49));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(208, 13));
			((Control)label7).set_TabIndex(1);
			((Control)label7).set_Text("Set the step interval to use in the DWD file");
			((Control)opt001Day).set_AutoSize(true);
			((Control)opt001Day).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)opt001Day).set_Location(new Point(155, 65));
			((Control)opt001Day).set_Name("opt001Day");
			((Control)opt001Day).set_Size(new Size(66, 17));
			((Control)opt001Day).set_TabIndex(4);
			((Control)opt001Day).set_Text("0.01 day");
			((ButtonBase)opt001Day).set_UseVisualStyleBackColor(true);
			((Control)opt01Day).set_AutoSize(true);
			((Control)opt01Day).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)opt01Day).set_Location(new Point(79, 65));
			((Control)opt01Day).set_Name("opt01Day");
			((Control)opt01Day).set_Size(new Size(60, 17));
			((Control)opt01Day).set_TabIndex(3);
			((Control)opt01Day).set_Text("0.1 day");
			((ButtonBase)opt01Day).set_UseVisualStyleBackColor(true);
			((Control)opt1Day).set_AutoSize(true);
			opt1Day.set_Checked(true);
			((Control)opt1Day).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)opt1Day).set_Location(new Point(12, 65));
			((Control)opt1Day).set_Name("opt1Day");
			((Control)opt1Day).set_Size(new Size(51, 17));
			((Control)opt1Day).set_TabIndex(2);
			opt1Day.set_TabStop(true);
			((Control)opt1Day).set_Text("1 day");
			((ButtonBase)opt1Day).set_UseVisualStyleBackColor(true);
			((Control)cmdDWDfile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDWDfile).set_Location(new Point(240, 49));
			((Control)cmdDWDfile).set_Name("cmdDWDfile");
			((Control)cmdDWDfile).set_Size(new Size(82, 33));
			((Control)cmdDWDfile).set_TabIndex(5);
			((Control)cmdDWDfile).set_Text("Create");
			((ButtonBase)cmdDWDfile).set_UseVisualStyleBackColor(true);
			((Control)cmdDWDfile).add_Click((EventHandler)cmdDWDfile_Click);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label13);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)updnLocalAltitude);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkLocalAltitude);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkShapeModel);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label12);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtSolarElong);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkSolarElongation);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label11);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label10);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkBinary);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)cmdReSetSites);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label6);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)cmbDay2);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)cmbDay1);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)cmbMonth2);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)cmbMonth1);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)updnYear2);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)updnYear1);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)cmbPlanet);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label5);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label4);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label3);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label2);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)label1);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtAsteroidName);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtAsteroidNumber);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtDia);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtDuration);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtMagDrop);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtDistance);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtKM);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtLatitude);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)txtLongitude);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkMagDrop);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkDuration);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkDiameter);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkAsteroidNumber);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkAsteroidName);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkDate);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkPlanet);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkTransNeptune);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkDistArcSec);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkSiteDistKM);
			((Control)grpSelectionCriteria).get_Controls().Add((Control)(object)chkSite);
			((Control)grpSelectionCriteria).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSelectionCriteria).set_Location(new Point(12, 69));
			((Control)grpSelectionCriteria).set_Name("grpSelectionCriteria");
			((Control)grpSelectionCriteria).set_Size(new Size(478, 518));
			((Control)grpSelectionCriteria).set_TabIndex(1);
			grpSelectionCriteria.set_TabStop(false);
			((Control)grpSelectionCriteria).set_Text("Selection criteria");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(411, 160));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(25, 13));
			((Control)label13).set_TabIndex(17);
			((Control)label13).set_Text("deg");
			((Control)updnLocalAltitude).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidLocalAltitudeValue", true, (DataSourceUpdateMode)1));
			((Control)updnLocalAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnLocalAltitude).set_Location(new Point(374, 157));
			updnLocalAltitude.set_Maximum(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnLocalAltitude.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLocalAltitude).set_Name("updnLocalAltitude");
			((Control)updnLocalAltitude).set_Size(new Size(34, 20));
			((Control)updnLocalAltitude).set_TabIndex(16);
			updnLocalAltitude.set_Value(Settings.Default.AsteroidLocalAltitudeValue);
			((Control)chkLocalAltitude).set_AutoSize(true);
			chkLocalAltitude.set_Checked(Settings.Default.AsteroidLocalAltitude);
			((Control)chkLocalAltitude).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidLocalAltitude", true, (DataSourceUpdateMode)1));
			((Control)chkLocalAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLocalAltitude).set_Location(new Point(280, 159));
			((Control)chkLocalAltitude).set_Name("chkLocalAltitude");
			((Control)chkLocalAltitude).set_Size(new Size(98, 17));
			((Control)chkLocalAltitude).set_TabIndex(15);
			((Control)chkLocalAltitude).set_Text("Local altitude >");
			((ButtonBase)chkLocalAltitude).set_UseVisualStyleBackColor(true);
			((Control)chkShapeModel).set_AutoSize(true);
			((Control)chkShapeModel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShapeModel).set_Location(new Point(279, 241));
			((Control)chkShapeModel).set_Name("chkShapeModel");
			((Control)chkShapeModel).set_Size(new Size(193, 17));
			((Control)chkShapeModel).set_TabIndex(27);
			((Control)chkShapeModel).set_Text("has a DAMIT or ISAM shape model");
			((ButtonBase)chkShapeModel).set_UseVisualStyleBackColor(true);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(417, 201));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(25, 13));
			((Control)label12).set_TabIndex(23);
			((Control)label12).set_Text("deg");
			((Control)txtSolarElong).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidSolarElong_deg", true, (DataSourceUpdateMode)1));
			((Control)txtSolarElong).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSolarElong).set_Location(new Point(387, 198));
			((Control)txtSolarElong).set_Name("txtSolarElong");
			((Control)txtSolarElong).set_Size(new Size(30, 20));
			((Control)txtSolarElong).set_TabIndex(22);
			((Control)txtSolarElong).set_Text(Settings.Default.AsteroidSolarElong_deg);
			((Control)chkSolarElongation).set_AutoSize(true);
			chkSolarElongation.set_Checked(Settings.Default.AsteroidSolarElongationChecked);
			((Control)chkSolarElongation).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSolarElongationChecked", true, (DataSourceUpdateMode)1));
			((Control)chkSolarElongation).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSolarElongation).set_Location(new Point(279, 200));
			((Control)chkSolarElongation).set_Name("chkSolarElongation");
			((Control)chkSolarElongation).set_Size(new Size(111, 17));
			((Control)chkSolarElongation).set_TabIndex(21);
			((Control)chkSolarElongation).set_Text("Solar elongation >");
			((ButtonBase)chkSolarElongation).set_UseVisualStyleBackColor(true);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_ForeColor(Color.MidnightBlue);
			((Control)label11).set_Location(new Point(41, 97));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(39, 17));
			((Control)label11).set_TabIndex(9);
			((Control)label11).set_Text("- or -");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_ForeColor(Color.MidnightBlue);
			((Control)label10).set_Location(new Point(290, 66));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(186, 39));
			((Control)label10).set_TabIndex(8);
			((Control)label10).set_Text("Distances above 500km are \r\napproximate. For distances >1000km, \r\nuse separation in arc-sec.");
			((Control)chkBinary).set_AutoSize(true);
			((Control)chkBinary).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkBinary).set_Location(new Point(23, 487));
			((Control)chkBinary).set_Name("chkBinary");
			((Control)chkBinary).set_Size(new Size(100, 17));
			((Control)chkBinary).set_TabIndex(43);
			((Control)chkBinary).set_Text("Binary asteroids");
			((ButtonBase)chkBinary).set_UseVisualStyleBackColor(true);
			((Control)cmdReSetSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReSetSites).set_Location(new Point(381, 30));
			((Control)cmdReSetSites).set_Name("cmdReSetSites");
			((Control)cmdReSetSites).set_Size(new Size(76, 24));
			((Control)cmdReSetSites).set_TabIndex(4);
			((Control)cmdReSetSites).set_Text("Select site");
			((ButtonBase)cmdReSetSites).set_UseVisualStyleBackColor(true);
			((Control)cmdReSetSites).add_Click((EventHandler)cmdReSetSites_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(261, 366));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(25, 13));
			((Control)label6).set_TabIndex(36);
			((Control)label6).set_Text("and");
			((Control)cmbDay2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDay2).set_FormattingEnabled(true);
			cmbDay2.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbDay2).set_Location(new Point(381, 362));
			((Control)cmbDay2).set_Name("cmbDay2");
			((Control)cmbDay2).set_Size(new Size(39, 21));
			((Control)cmbDay2).set_TabIndex(39);
			((Control)cmbDay1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDay1).set_FormattingEnabled(true);
			cmbDay1.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbDay1).set_Location(new Point(221, 362));
			((Control)cmbDay1).set_Name("cmbDay1");
			((Control)cmbDay1).set_Size(new Size(40, 21));
			((Control)cmbDay1).set_TabIndex(35);
			cmbDay1.add_SelectedIndexChanged((EventHandler)cmbDay1_SelectedIndexChanged);
			((Control)cmbMonth2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMonth2).set_FormattingEnabled(true);
			cmbMonth2.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbMonth2).set_Location(new Point(337, 362));
			((Control)cmbMonth2).set_Name("cmbMonth2");
			((Control)cmbMonth2).set_Size(new Size(44, 21));
			((Control)cmbMonth2).set_TabIndex(38);
			((Control)cmbMonth1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMonth1).set_FormattingEnabled(true);
			cmbMonth1.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbMonth1).set_Location(new Point(178, 362));
			((Control)cmbMonth1).set_Name("cmbMonth1");
			((Control)cmbMonth1).set_Size(new Size(43, 21));
			((Control)cmbMonth1).set_TabIndex(34);
			cmbMonth1.add_SelectedIndexChanged((EventHandler)cmbMonth1_SelectedIndexChanged);
			((Control)updnYear2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnYear2).set_Location(new Point(286, 362));
			updnYear2.set_Maximum(new decimal(new int[4] { 2200, 0, 0, 0 }));
			updnYear2.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnYear2).set_Name("updnYear2");
			((Control)updnYear2).set_Size(new Size(51, 20));
			((Control)updnYear2).set_TabIndex(37);
			updnYear2.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnYear2).add_Enter((EventHandler)updnYear2_Enter);
			((Control)updnYear1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnYear1).set_Location(new Point(127, 362));
			updnYear1.set_Maximum(new decimal(new int[4] { 2200, 0, 0, 0 }));
			updnYear1.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnYear1).set_Name("updnYear1");
			((Control)updnYear1).set_Size(new Size(51, 20));
			((Control)updnYear1).set_TabIndex(33);
			updnYear1.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			updnYear1.add_ValueChanged((EventHandler)updnYear1_ValueChanged);
			((Control)updnYear1).add_Enter((EventHandler)updnYear1_Enter);
			((Control)cmbPlanet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPlanet).set_FormattingEnabled(true);
			cmbPlanet.get_Items().AddRange(new object[8] { "Mercury", "Venus", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" });
			((Control)cmbPlanet).set_Location(new Point(215, 403));
			((Control)cmbPlanet).set_Name("cmbPlanet");
			((Control)cmbPlanet).set_Size(new Size(69, 21));
			((Control)cmbPlanet).set_TabIndex(41);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(161, 243));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(21, 13));
			((Control)label5).set_TabIndex(26);
			((Control)label5).set_Text("km");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(200, 202));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(20);
			((Control)label4).set_Text("secs");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(262, 120));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(42, 13));
			((Control)label3).set_TabIndex(12);
			((Control)label3).set_Text("arc-sec");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(263, 79));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(21, 13));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("km");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(227, 38));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(66, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("and Latitude");
			((Control)txtAsteroidName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroidName).set_Location(new Point(162, 321));
			((Control)txtAsteroidName).set_Name("txtAsteroidName");
			((Control)txtAsteroidName).set_Size(new Size(76, 20));
			((Control)txtAsteroidName).set_TabIndex(31);
			((Control)txtAsteroidName).add_Enter((EventHandler)txtAsteroidName_Enter);
			((Control)txtAsteroidNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroidNumber).set_Location(new Point(170, 280));
			((Control)txtAsteroidNumber).set_Name("txtAsteroidNumber");
			((Control)txtAsteroidNumber).set_Size(new Size(47, 20));
			((Control)txtAsteroidNumber).set_TabIndex(29);
			((Control)txtAsteroidNumber).add_Enter((EventHandler)txtAsteroidNumber_Enter);
			((Control)txtDia).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidDiameter", true, (DataSourceUpdateMode)1));
			((Control)txtDia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDia).set_Location(new Point(131, 239));
			((Control)txtDia).set_Name("txtDia");
			((Control)txtDia).set_Size(new Size(30, 20));
			((Control)txtDia).set_TabIndex(25);
			((Control)txtDia).set_Text(Settings.Default.AsteroidDiameter);
			((Control)txtDia).add_Enter((EventHandler)txtDia_Enter);
			((Control)txtDuration).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidDuration", true, (DataSourceUpdateMode)1));
			((Control)txtDuration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDuration).set_Location(new Point(172, 198));
			((Control)txtDuration).set_Name("txtDuration");
			((Control)txtDuration).set_Size(new Size(28, 20));
			((Control)txtDuration).set_TabIndex(19);
			((Control)txtDuration).set_Text(Settings.Default.AsteroidDuration);
			((Control)txtDuration).add_Enter((EventHandler)txtDuration_Enter);
			((Control)txtMagDrop).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidMagDrop", true, (DataSourceUpdateMode)1));
			((Control)txtMagDrop).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMagDrop).set_Location(new Point(228, 157));
			((Control)txtMagDrop).set_Name("txtMagDrop");
			((Control)txtMagDrop).set_Size(new Size(27, 20));
			((Control)txtMagDrop).set_TabIndex(14);
			((Control)txtMagDrop).set_Text(Settings.Default.AsteroidMagDrop);
			((Control)txtMagDrop).add_Enter((EventHandler)txtMagDrop_Enter);
			((Control)txtDistance).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidDistanceSec", true, (DataSourceUpdateMode)1));
			((Control)txtDistance).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDistance).set_Location(new Point(233, 116));
			((Control)txtDistance).set_Name("txtDistance");
			((Control)txtDistance).set_Size(new Size(29, 20));
			((Control)txtDistance).set_TabIndex(11);
			((Control)txtDistance).set_Text(Settings.Default.AsteroidDistanceSec);
			((Control)txtDistance).add_Enter((EventHandler)txtDistance_Enter);
			((Control)txtKM).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidDistanceKM", true, (DataSourceUpdateMode)1));
			((Control)txtKM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtKM).set_Location(new Point(216, 75));
			((Control)txtKM).set_Name("txtKM");
			((Control)txtKM).set_Size(new Size(47, 20));
			((Control)txtKM).set_TabIndex(6);
			((Control)txtKM).set_Text(Settings.Default.AsteroidDistanceKM);
			((Control)txtKM).add_Enter((EventHandler)txtKM_Enter);
			((Control)txtLatitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatitude).set_Location(new Point(293, 34));
			((Control)txtLatitude).set_Name("txtLatitude");
			((Control)txtLatitude).set_Size(new Size(55, 20));
			((Control)txtLatitude).set_TabIndex(3);
			((Control)txtLatitude).set_Text(Settings.Default.AsteroidSiteLatitude);
			((Control)txtLatitude).add_Enter((EventHandler)txtLatitude_Enter);
			((Control)txtLongitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongitude).set_Location(new Point(165, 34));
			((Control)txtLongitude).set_Name("txtLongitude");
			((Control)txtLongitude).set_Size(new Size(55, 20));
			((Control)txtLongitude).set_TabIndex(1);
			((Control)txtLongitude).set_Text(Settings.Default.AsteroidSiteLongitude);
			((Control)txtLongitude).add_Enter((EventHandler)txtLongitude_Enter);
			((Control)chkMagDrop).set_AutoSize(true);
			chkMagDrop.set_Checked(Settings.Default.AsteroidMagDropChecked);
			((Control)chkMagDrop).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidMagDropChecked", true, (DataSourceUpdateMode)1));
			((Control)chkMagDrop).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMagDrop).set_Location(new Point(23, 159));
			((Control)chkMagDrop).set_Name("chkMagDrop");
			((Control)chkMagDrop).set_Size(new Size(204, 17));
			((Control)chkMagDrop).set_TabIndex(13);
			((Control)chkMagDrop).set_Text("Magnitude drop at occultation at least");
			((ButtonBase)chkMagDrop).set_UseVisualStyleBackColor(true);
			((Control)chkDuration).set_AutoSize(true);
			chkDuration.set_Checked(Settings.Default.AsteroidDurationChecked);
			((Control)chkDuration).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDurationChecked", true, (DataSourceUpdateMode)1));
			((Control)chkDuration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDuration).set_Location(new Point(23, 200));
			((Control)chkDuration).set_Name("chkDuration");
			((Control)chkDuration).set_Size(new Size(148, 17));
			((Control)chkDuration).set_TabIndex(18);
			((Control)chkDuration).set_Text("Maximum duration at least");
			((ButtonBase)chkDuration).set_UseVisualStyleBackColor(true);
			((Control)chkDiameter).set_AutoSize(true);
			chkDiameter.set_Checked(Settings.Default.AsteroidDiameterChecked);
			((Control)chkDiameter).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDiameterChecked", true, (DataSourceUpdateMode)1));
			((Control)chkDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDiameter).set_Location(new Point(23, 241));
			((Control)chkDiameter).set_Name("chkDiameter");
			((Control)chkDiameter).set_Size(new Size(105, 17));
			((Control)chkDiameter).set_TabIndex(24);
			((Control)chkDiameter).set_Text("Diameter at least");
			((ButtonBase)chkDiameter).set_UseVisualStyleBackColor(true);
			((Control)chkAsteroidNumber).set_AutoSize(true);
			((Control)chkAsteroidNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAsteroidNumber).set_Location(new Point(23, 282));
			((Control)chkAsteroidNumber).set_Name("chkAsteroidNumber");
			((Control)chkAsteroidNumber).set_Size(new Size(145, 17));
			((Control)chkAsteroidNumber).set_TabIndex(28);
			((Control)chkAsteroidNumber).set_Text("Asteroid number contains");
			((ButtonBase)chkAsteroidNumber).set_UseVisualStyleBackColor(true);
			((Control)chkAsteroidName).set_AutoSize(true);
			((Control)chkAsteroidName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAsteroidName).set_Location(new Point(23, 323));
			((Control)chkAsteroidName).set_Name("chkAsteroidName");
			((Control)chkAsteroidName).set_Size(new Size(136, 17));
			((Control)chkAsteroidName).set_TabIndex(30);
			((Control)chkAsteroidName).set_Text("Asteroid name contains");
			((ButtonBase)chkAsteroidName).set_UseVisualStyleBackColor(true);
			((Control)chkDate).set_AutoSize(true);
			chkDate.set_Checked(Settings.Default.AsteroidDateRangeChecked);
			((Control)chkDate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDate).set_Location(new Point(23, 364));
			((Control)chkDate).set_Name("chkDate");
			((Control)chkDate).set_Size(new Size(93, 17));
			((Control)chkDate).set_TabIndex(32);
			((Control)chkDate).set_Text("Date between");
			((ButtonBase)chkDate).set_UseVisualStyleBackColor(true);
			((Control)chkPlanet).set_AutoSize(true);
			chkPlanet.set_Checked(Settings.Default.AsteroidPlanetChecked);
			((Control)chkPlanet).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidPlanetChecked", true, (DataSourceUpdateMode)1));
			((Control)chkPlanet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPlanet).set_Location(new Point(23, 405));
			((Control)chkPlanet).set_Name("chkPlanet");
			((Control)chkPlanet).set_Size(new Size(183, 17));
			((Control)chkPlanet).set_TabIndex(40);
			((Control)chkPlanet).set_Text("Planet (for planets or their moons)");
			((ButtonBase)chkPlanet).set_UseVisualStyleBackColor(true);
			((Control)chkTransNeptune).set_AutoSize(true);
			chkTransNeptune.set_Checked(Settings.Default.AsteroidTNO);
			((Control)chkTransNeptune).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidTNO", true, (DataSourceUpdateMode)1));
			((Control)chkTransNeptune).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkTransNeptune).set_Location(new Point(23, 446));
			((Control)chkTransNeptune).set_Name("chkTransNeptune");
			((Control)chkTransNeptune).set_Size(new Size(207, 17));
			((Control)chkTransNeptune).set_TabIndex(42);
			((Control)chkTransNeptune).set_Text("Trans-Neptunian asteroids [ a > 6 AU ]");
			((ButtonBase)chkTransNeptune).set_UseVisualStyleBackColor(true);
			((Control)chkDistArcSec).set_AutoSize(true);
			chkDistArcSec.set_Checked(Settings.Default.AsteroidDistanceSecChecked);
			((Control)chkDistArcSec).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDistanceSecChecked", true, (DataSourceUpdateMode)1));
			((Control)chkDistArcSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDistArcSec).set_Location(new Point(23, 118));
			((Control)chkDistArcSec).set_Name("chkDistArcSec");
			((Control)chkDistArcSec).set_Size(new Size(208, 17));
			((Control)chkDistArcSec).set_TabIndex(10);
			((Control)chkDistArcSec).set_Text("Distance of asteroid from star less than");
			((ButtonBase)chkDistArcSec).set_UseVisualStyleBackColor(true);
			((Control)chkSiteDistKM).set_AutoSize(true);
			chkSiteDistKM.set_Checked(Settings.Default.AsteroidDistanceKMChecked);
			chkSiteDistKM.set_CheckState((CheckState)1);
			((Control)chkSiteDistKM).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDistanceKMChecked", true, (DataSourceUpdateMode)1));
			((Control)chkSiteDistKM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSiteDistKM).set_Location(new Point(23, 77));
			((Control)chkSiteDistKM).set_Name("chkSiteDistKM");
			((Control)chkSiteDistKM).set_Size(new Size(191, 17));
			((Control)chkSiteDistKM).set_TabIndex(5);
			((Control)chkSiteDistKM).set_Text("Distance of site from path less than");
			((ButtonBase)chkSiteDistKM).set_UseVisualStyleBackColor(true);
			((Control)chkSite).set_AutoSize(true);
			chkSite.set_Checked(Settings.Default.AsteroidSiteChecked);
			chkSite.set_CheckState((CheckState)1);
			((Control)chkSite).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSiteChecked", true, (DataSourceUpdateMode)1));
			((Control)chkSite).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSite).set_Location(new Point(23, 36));
			((Control)chkSite).set_Name("chkSite");
			((Control)chkSite).set_Size(new Size(139, 17));
			((Control)chkSite).set_TabIndex(0);
			((Control)chkSite).set_Text("Visible from E.Longitude");
			((ButtonBase)chkSite).set_UseVisualStyleBackColor(true);
			chkSite.add_CheckedChanged((EventHandler)chkSite_CheckedChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(30, 24));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(447, 26));
			((Control)label8).set_TabIndex(0);
			((Control)label8).set_Text("Display asteroidal occultation predictions");
			((Control)groupBox1).get_Controls().Add((Control)(object)updnScale);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(517, 417));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(353, 56));
			((Control)groupBox1).set_TabIndex(4);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Plot scale for world maps");
			((Control)updnScale).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "PlotScale", true, (DataSourceUpdateMode)1));
			((Control)updnScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnScale).set_Location(new Point(55, 28));
			updnScale.set_Maximum(new decimal(new int[4] { 16, 0, 0, 0 }));
			updnScale.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnScale).set_Name("updnScale");
			((UpDownBase)updnScale).set_ReadOnly(true);
			((Control)updnScale).set_Size(new Size(50, 20));
			((Control)updnScale).set_TabIndex(0);
			updnScale.set_Value(Settings.Default.PlotScale);
			((Control)updnScale).add_Click((EventHandler)updnScale_Click);
			((Control)updnScale).add_DoubleClick((EventHandler)updnScale_DoubleClick);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkAutoGenerate);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdPlanetEventTimes);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkSaveElements);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdPlotMultiPaths);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdGlobalSummaryOfEvents);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdSummaryOfEvents);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(517, 47));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(353, 248));
			((Control)groupBox2).set_TabIndex(2);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Use summary list to select events");
			((Control)chkAutoGenerate).set_AutoSize(true);
			chkAutoGenerate.set_Checked(Settings.Default.AsteroidalsAutoPredictFutureOutputs);
			((Control)chkAutoGenerate).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidalsAutoPredictFutureOutputs", true, (DataSourceUpdateMode)1));
			((Control)chkAutoGenerate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAutoGenerate).set_Location(new Point(18, 28));
			((Control)chkAutoGenerate).set_Name("chkAutoGenerate");
			((Control)chkAutoGenerate).set_Size(new Size(288, 17));
			((Control)chkAutoGenerate).set_TabIndex(0);
			((Control)chkAutoGenerate).set_Text("Enable generation of output files from the Summary form");
			((ButtonBase)chkAutoGenerate).set_UseVisualStyleBackColor(true);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdPlotEvents);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(517, 303));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(353, 106));
			((Control)groupBox3).set_TabIndex(3);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Plot events in sequence");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(884, 24));
			((Control)menuStrip1).set_TabIndex(6);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(90, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help          ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(884, 595));
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)grpSelectionCriteria);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("SelectAndDisplay");
			((Control)this).set_Text("Display asteroid occultation predictions");
			((Form)this).add_Load((EventHandler)SelectAndDisplay_Load);
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)grpSelectionCriteria).ResumeLayout(false);
			((Control)grpSelectionCriteria).PerformLayout();
			((ISupportInitialize)updnLocalAltitude).EndInit();
			((ISupportInitialize)updnYear2).EndInit();
			((ISupportInitialize)updnYear1).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((ISupportInitialize)updnScale).EndInit();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
