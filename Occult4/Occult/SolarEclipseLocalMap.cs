using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.BailyBeads;
using Occult.Properties;

namespace Occult
{
	public class SolarEclipseLocalMap : Form
	{
		private static bool ShowToolTip = true;

		private static bool FormStarted = false;

		private bool GetToolTip = true;

		private static int MouseX = 0;

		private static int MouseY = 0;

		private readonly string AppPath;

		private double Longitude;

		private double Latitude;

		internal static bool MouseMoveDisabled = false;

		private IContainer components;

		private MenuStrip menuStrip1;

		private Button cmdRePlot;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		public NumericUpDown updnSouth;

		public NumericUpDown updnNorth;

		public NumericUpDown updnEast;

		public NumericUpDown updnWest;

		public PictureBox picLocalMap;

		private ToolTip toolTip1;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem bWToolStripMenuItem;

		private ToolStripMenuItem colourToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem detailedPredictionsToolStripMenuItem;

		private ToolStripMenuItem localPredictionToolStripMenuItem;

		private ToolStripMenuItem multiLocationPredictionToolStripMenuItem;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private Label label5;

		private Label label6;

		internal ComboBox cmbSiteFiles;

		internal ComboBox cmbNames;

		private Label label7;

		private ContextMenuStrip contextMenuStrip1;

		private Label label8;

		private ToolStripMenuItem localPredictionsToolStripMenuItem;

		private ToolStripMenuItem multilocationPredictionsToolStripMenuItem;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem viewInGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem saveGoogleEarthKMLFileToolStripMenuItem;

		private ToolStripMenuItem saveGoogleMapsHTMFileToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem setupBailyBeadsForAnimationToolStripMenuItem;

		private ToolStripMenuItem viewBesselianElementsToolStripMenuItem;

		private ToolStripMenuItem lunarProfileToolStripMenuItem;

		public SolarEclipseLocalMap()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void SolarEclipseLocalMap_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			FormStarted = false;
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(SolarEclipses.SolarEclipseLabel + " - detailed map");
			bWToolStripMenuItem.set_Checked(Settings.Default.BWFlag);
			colourToolStripMenuItem.set_Checked(!bWToolStripMenuItem.get_Checked());
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
			}
			cmbSiteFiles.get_Items().Add((object)"   don't plot");
			((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			string siteFileForEclipseMap = Settings.Default.SiteFileForEclipseMap;
			for (int j = 0; j < cmbSiteFiles.get_Items().get_Count(); j++)
			{
				if (siteFileForEclipseMap == cmbSiteFiles.get_Items().get_Item(j).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(j);
					break;
				}
			}
			((ListControl)cmbNames).set_SelectedIndex(0);
			FormStarted = true;
		}

		private void SolarEclipseLocalMap_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			Settings.Default.SolarEclipseLocalMapWindow = ((Form)this).get_WindowState();
		}

		private void cmdRePlot_Click(object sender, EventArgs e)
		{
			PlotMap();
		}

		public void PlotMap()
		{
			if (FormStarted)
			{
				SolarEclipses.PlotLocalMap(Settings.Default.BWFlag);
				((Control)this).Focus();
			}
		}

		private void SolarEclipseLocalMap_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 50) | (((Control)this).get_Height() < 120)))
			{
				((Control)picLocalMap).set_Width(((Control)this).get_Width() - 27);
				((Control)picLocalMap).set_Height(((Control)this).get_Height() - 113);
				PlotMap();
			}
		}

		private void picLocalMap_MouseMove(object sender, MouseEventArgs e)
		{
			if (MouseMoveDisabled)
			{
				return;
			}
			Maps.InverseMapProjection(out Longitude, out Latitude, e.get_X(), e.get_Y());
			MouseX = e.get_X();
			MouseY = e.get_Y();
			if (ShowToolTip)
			{
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip1.SetToolTip((Control)(object)picLocalMap, Longitude.ToString("+0.0  ;-0.0  ") + Latitude.ToString("+0.0;-0.0"));
				}
			}
		}

		private void picLocalMap_DoubleClick(object sender, EventArgs e)
		{
			MouseMoveDisabled = true;
			Maps.InverseMapProjection(out var Longitude_deg, out var Latitude_deg, MouseX, MouseY);
			SolarEclipses.MidLongitude_deg = Longitude_deg;
			SolarEclipses.MidLatitude_deg = Latitude_deg;
			SolarEclipses.Range_deg = 30.0;
			SolarEclipses.ShowLocalMap(UseCursorPosition: true);
			MouseMoveDisabled = false;
		}

		private void bWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = bWToolStripMenuItem;
			bool @checked = (Settings.Default.BWFlag = true);
			obj.set_Checked(@checked);
			colourToolStripMenuItem.set_Checked(!bWToolStripMenuItem.get_Checked());
			PlotMap();
		}

		private void colourToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = bWToolStripMenuItem;
			bool @checked = (Settings.Default.BWFlag = false);
			obj.set_Checked(@checked);
			colourToolStripMenuItem.set_Checked(!bWToolStripMenuItem.get_Checked());
			PlotMap();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picLocalMap.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.PrintPreviewLocalGraphic();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.PrintLocalGraphic();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_SolarEclipses = Output.SaveGraphic(picLocalMap.get_Image(), SolarEclipses.SolarEclipseLabel + "Local Plot", Settings.Default.Save_SolarEclipses);
		}

		private void localPredictionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowSingleLocation();
		}

		private void multiLocationPredictionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_Multilocations();
		}

		private void pathCoordinatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_PathCoordinates();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void cmbSiteFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormStarted)
			{
				Settings.Default.SiteFileForEclipseMap = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
				PlotMap();
			}
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormStarted)
			{
				PlotMap();
			}
		}

		private void localPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			MouseMoveDisabled = true;
			SolarEclipses.LocalLongitude = Longitude;
			SolarEclipses.LocalLatitude = Latitude;
			SolarEclipses.LocalAltitude = 0.0;
			SolarEclipses.LocalName = "Selected location";
			SolarEclipses.ShowSingleLocation();
			((Control)SolarEclipses.SingleLocation).set_Text(SolarEclipses.SolarEclipseLabel + " - Local predictions");
		}

		private void multilocationPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_Multilocations();
		}

		private void pathCoordinatesToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_PathCoordinates();
		}

		private void viewInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				SolarEclipses.GoogleEarthCurves(View: true);
			}
		}

		private void saveGoogleEarthKMLFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleEarthCurves(View: false);
		}

		private void saveGoogleMapsHTMFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.GoogleMapCurves();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solar eclipse - detailed map");
		}

		private void setupBailyBeadsForAnimationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Occult.BailyBeads.BailyBeads.Show_Main();
			Occult.BailyBeads.BailyBeads.Show_Eclipse_Image();
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtLongDDD).set_Text(string.Format("{0,1:F5}", Longitude));
			Occult.BailyBeads.BailyBeads.BailyBeads_Main.UpdateLongitudes(2);
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtLatDDD).set_Text(string.Format("{0,1:F5}", Latitude));
			Occult.BailyBeads.BailyBeads.BailyBeads_Main.UpdateLatitudes(2);
			SolarEclipses.LocalCircumstances("", Longitude, Latitude, 0.0, IncludeLimbCorrections: false);
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtYear).set_Text(SolarEclipses.CurrentYear.ToString());
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtMonth).set_Text(SolarEclipses.CurrentMonth.ToString());
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtDay).set_Text(SolarEclipses.CurrentDay.ToString());
			if (SolarEclipses.DayFlag[2] == "+ ")
			{
				((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtDay).set_Text((SolarEclipses.CurrentDay + 1.0).ToString());
			}
			if (SolarEclipses.DayFlag[2] == "- ")
			{
				((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtDay).set_Text((SolarEclipses.CurrentDay - 1.0).ToString());
			}
			Occult.BailyBeads.BailyBeads.BailyBeads_Main.chkAnimation.set_Checked(true);
			if (Occult.BailyBeads.BailyBeads.BailyBeads_Main.updnMagnification.get_Value() == 1m)
			{
				Occult.BailyBeads.BailyBeads.BailyBeads_Main.updnMagnification.set_Value(0.4m);
			}
			if (Occult.BailyBeads.BailyBeads.BailyBeads_Main.updnAnimateDuration.get_Value() == 0m)
			{
				Occult.BailyBeads.BailyBeads.BailyBeads_Main.updnAnimateDuration.set_Value(300m);
				Occult.BailyBeads.BailyBeads.BailyBeads_Main.updnAnimateStep.set_Value(2m);
			}
			string text = Utilities.DEGtoDMS(SolarEclipses.ContactTimes[2] - 0.03, 3, 0, MinutesOnly: false);
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtHr).set_Text(text.Substring(0, 3).Trim());
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main.txtMin).set_Text(text.Substring(4, 2).Trim());
			((Control)Occult.BailyBeads.BailyBeads.BailyBeads_Main).Focus();
			((Control)Occult.BailyBeads.BailyBeads.Eclipse_Image).Focus();
			Occult.BailyBeads.BailyBeads.BailyBeads_Main.cmdPlot_Click(sender, e);
		}

		private void viewBesselianElementsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ListBesselianElements();
		}

		private void lunarProfileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			MouseMoveDisabled = true;
			SolarEclipses.ContactTimes[2] = -10.0;
			SolarEclipses.GenerateLunarProfile(Longitude, Latitude, SaveFile: true);
			MouseMoveDisabled = false;
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
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
			//IL_017d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Expected O, but got Unknown
			//IL_0188: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Expected O, but got Unknown
			//IL_0193: Unknown result type (might be due to invalid IL or missing references)
			//IL_019d: Expected O, but got Unknown
			//IL_01a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ae: Expected O, but got Unknown
			//IL_01af: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b9: Expected O, but got Unknown
			//IL_01ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c4: Expected O, but got Unknown
			//IL_01c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01cf: Expected O, but got Unknown
			//IL_01d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01da: Expected O, but got Unknown
			//IL_01db: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e5: Expected O, but got Unknown
			//IL_01e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f0: Expected O, but got Unknown
			//IL_0e03: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e0d: Expected O, but got Unknown
			//IL_1670: Unknown result type (might be due to invalid IL or missing references)
			//IL_167a: Expected O, but got Unknown
			//IL_1691: Unknown result type (might be due to invalid IL or missing references)
			//IL_169b: Expected O, but got Unknown
			//IL_16eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_170f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1719: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			viewBesselianElementsToolStripMenuItem = new ToolStripMenuItem();
			bWToolStripMenuItem = new ToolStripMenuItem();
			colourToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			detailedPredictionsToolStripMenuItem = new ToolStripMenuItem();
			localPredictionToolStripMenuItem = new ToolStripMenuItem();
			multiLocationPredictionToolStripMenuItem = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdRePlot = new Button();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			updnSouth = new NumericUpDown();
			updnNorth = new NumericUpDown();
			updnEast = new NumericUpDown();
			updnWest = new NumericUpDown();
			picLocalMap = new PictureBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			localPredictionsToolStripMenuItem = new ToolStripMenuItem();
			multilocationPredictionsToolStripMenuItem = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem1 = new ToolStripMenuItem();
			setupBailyBeadsForAnimationToolStripMenuItem = new ToolStripMenuItem();
			lunarProfileToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			viewInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleEarthKMLFileToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleMapsHTMFileToolStripMenuItem = new ToolStripMenuItem();
			toolTip1 = new ToolTip(components);
			label5 = new Label();
			label6 = new Label();
			cmbSiteFiles = new ComboBox();
			cmbNames = new ComboBox();
			label7 = new Label();
			label8 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnSouth).BeginInit();
			((ISupportInitialize)updnNorth).BeginInit();
			((ISupportInitialize)updnEast).BeginInit();
			((ISupportInitialize)updnWest).BeginInit();
			((ISupportInitialize)picLocalMap).BeginInit();
			((Control)contextMenuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)detailedPredictionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(836, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)viewBesselianElementsToolStripMenuItem,
				(ToolStripItem)bWToolStripMenuItem,
				(ToolStripItem)colourToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...");
			((ToolStripItem)viewBesselianElementsToolStripMenuItem).set_Image((Image)Resources.ShowGridlines2HS);
			((ToolStripItem)viewBesselianElementsToolStripMenuItem).set_Name("viewBesselianElementsToolStripMenuItem");
			((ToolStripItem)viewBesselianElementsToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)viewBesselianElementsToolStripMenuItem).set_Text("List Besselian elements");
			((ToolStripItem)viewBesselianElementsToolStripMenuItem).add_Click((EventHandler)viewBesselianElementsToolStripMenuItem_Click);
			((ToolStripItem)bWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)bWToolStripMenuItem).set_Name("bWToolStripMenuItem");
			((ToolStripItem)bWToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)bWToolStripMenuItem).set_Text("B&&W");
			((ToolStripItem)bWToolStripMenuItem).add_Click((EventHandler)bWToolStripMenuItem_Click);
			colourToolStripMenuItem.set_Checked(true);
			colourToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)colourToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)colourToolStripMenuItem).set_Name("colourToolStripMenuItem");
			((ToolStripItem)colourToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)colourToolStripMenuItem).set_Text("Colour");
			((ToolStripItem)colourToolStripMenuItem).add_Click((EventHandler)colourToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(191, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)detailedPredictionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)localPredictionToolStripMenuItem,
				(ToolStripItem)multiLocationPredictionToolStripMenuItem,
				(ToolStripItem)pathCoordinatesToolStripMenuItem
			});
			((ToolStripItem)detailedPredictionsToolStripMenuItem).set_Name("detailedPredictionsToolStripMenuItem");
			((ToolStripItem)detailedPredictionsToolStripMenuItem).set_Size(new Size(133, 20));
			((ToolStripItem)detailedPredictionsToolStripMenuItem).set_Text("Detailed predictions...");
			((ToolStripItem)localPredictionToolStripMenuItem).set_Name("localPredictionToolStripMenuItem");
			((ToolStripItem)localPredictionToolStripMenuItem).set_Size(new Size(205, 22));
			((ToolStripItem)localPredictionToolStripMenuItem).set_Text("Local prediction");
			((ToolStripItem)localPredictionToolStripMenuItem).add_Click((EventHandler)localPredictionToolStripMenuItem_Click);
			((ToolStripItem)multiLocationPredictionToolStripMenuItem).set_Name("multiLocationPredictionToolStripMenuItem");
			((ToolStripItem)multiLocationPredictionToolStripMenuItem).set_Size(new Size(205, 22));
			((ToolStripItem)multiLocationPredictionToolStripMenuItem).set_Text("MultiLocation prediction");
			((ToolStripItem)multiLocationPredictionToolStripMenuItem).add_Click((EventHandler)multiLocationPredictionToolStripMenuItem_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Name("pathCoordinatesToolStripMenuItem");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Size(new Size(205, 22));
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).add_Click((EventHandler)pathCoordinatesToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdRePlot).set_Anchor((AnchorStyles)1);
			((Control)cmdRePlot).set_Location(new Point(699, 32));
			((Control)cmdRePlot).set_Name("cmdRePlot");
			((Control)cmdRePlot).set_Size(new Size(70, 28));
			((Control)cmdRePlot).set_TabIndex(18);
			((Control)cmdRePlot).set_Text("&Draw map");
			((ButtonBase)cmdRePlot).set_UseVisualStyleBackColor(true);
			((Control)cmdRePlot).add_Click((EventHandler)cmdRePlot_Click);
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(574, 38));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(35, 13));
			((Control)label4).set_TabIndex(16);
			((Control)label4).set_Text("South");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(476, 38));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(33, 13));
			((Control)label3).set_TabIndex(14);
			((Control)label3).set_Text("North");
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(336, 38));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(28, 13));
			((Control)label2).set_TabIndex(12);
			((Control)label2).set_Text("East");
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(227, 38));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(32, 13));
			((Control)label1).set_TabIndex(10);
			((Control)label1).set_Text("West");
			((Control)updnSouth).set_Anchor((AnchorStyles)1);
			updnSouth.set_DecimalPlaces(1);
			((Control)updnSouth).set_Location(new Point(609, 34));
			updnSouth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnSouth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnSouth).set_Name("updnSouth");
			((Control)updnSouth).set_Size(new Size(48, 20));
			((Control)updnSouth).set_TabIndex(17);
			((Control)updnNorth).set_Anchor((AnchorStyles)1);
			updnNorth.set_DecimalPlaces(1);
			((Control)updnNorth).set_Location(new Point(509, 34));
			updnNorth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnNorth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnNorth).set_Name("updnNorth");
			((Control)updnNorth).set_Size(new Size(52, 20));
			((Control)updnNorth).set_TabIndex(15);
			((Control)updnEast).set_Anchor((AnchorStyles)1);
			updnEast.set_DecimalPlaces(1);
			((Control)updnEast).set_Location(new Point(364, 34));
			updnEast.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnEast.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnEast).set_Name("updnEast");
			((Control)updnEast).set_Size(new Size(65, 20));
			((Control)updnEast).set_TabIndex(13);
			((Control)updnWest).set_Anchor((AnchorStyles)1);
			updnWest.set_DecimalPlaces(1);
			((Control)updnWest).set_Location(new Point(259, 34));
			updnWest.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnWest.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnWest).set_Name("updnWest");
			((Control)updnWest).set_Size(new Size(65, 20));
			((Control)updnWest).set_TabIndex(11);
			((Control)picLocalMap).set_ContextMenuStrip(contextMenuStrip1);
			((Control)picLocalMap).set_Location(new Point(9, 68));
			((Control)picLocalMap).set_Name("picLocalMap");
			((Control)picLocalMap).set_Size(new Size(817, 523));
			picLocalMap.set_TabIndex(19);
			picLocalMap.set_TabStop(false);
			((Control)picLocalMap).add_DoubleClick((EventHandler)picLocalMap_DoubleClick);
			((Control)picLocalMap).add_MouseMove(new MouseEventHandler(picLocalMap_MouseMove));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)localPredictionsToolStripMenuItem,
				(ToolStripItem)multilocationPredictionsToolStripMenuItem,
				(ToolStripItem)pathCoordinatesToolStripMenuItem1,
				(ToolStripItem)setupBailyBeadsForAnimationToolStripMenuItem,
				(ToolStripItem)lunarProfileToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)viewInGoogleEarthToolStripMenuItem,
				(ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem,
				(ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(244, 208));
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Name("localPredictionsToolStripMenuItem");
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Text("Local predictions");
			((ToolStripItem)localPredictionsToolStripMenuItem).add_Click((EventHandler)localPredictionsToolStripMenuItem_Click);
			((ToolStripItem)multilocationPredictionsToolStripMenuItem).set_Image((Image)Resources.MultiLocation);
			((ToolStripItem)multilocationPredictionsToolStripMenuItem).set_Name("multilocationPredictionsToolStripMenuItem");
			((ToolStripItem)multilocationPredictionsToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)multilocationPredictionsToolStripMenuItem).set_Text("Multilocation predictions");
			((ToolStripItem)multilocationPredictionsToolStripMenuItem).add_Click((EventHandler)multilocationPredictionsToolStripMenuItem_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Image((Image)Resources.Paths);
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Name("pathCoordinatesToolStripMenuItem1");
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Size(new Size(243, 22));
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem1).add_Click((EventHandler)pathCoordinatesToolStripMenuItem1_Click);
			((ToolStripItem)setupBailyBeadsForAnimationToolStripMenuItem).set_Name("setupBailyBeadsForAnimationToolStripMenuItem");
			((ToolStripItem)setupBailyBeadsForAnimationToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)setupBailyBeadsForAnimationToolStripMenuItem).set_Text("Set-up BailyBeads for animation");
			((ToolStripItem)setupBailyBeadsForAnimationToolStripMenuItem).add_Click((EventHandler)setupBailyBeadsForAnimationToolStripMenuItem_Click);
			((ToolStripItem)lunarProfileToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)lunarProfileToolStripMenuItem).set_Name("lunarProfileToolStripMenuItem");
			((ToolStripItem)lunarProfileToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)lunarProfileToolStripMenuItem).set_Text(" Lunar profile - generate && save");
			((ToolStripItem)lunarProfileToolStripMenuItem).add_Click((EventHandler)lunarProfileToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(240, 6));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Name("viewInGoogleEarthToolStripMenuItem");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Text("View in GoogleEarth");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).add_Click((EventHandler)viewInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Image((Image)Resources.kml_file);
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Name("saveGoogleEarthKMLFileToolStripMenuItem");
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Text("Save GoogleEarth KML file");
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).add_Click((EventHandler)saveGoogleEarthKMLFileToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Name("saveGoogleMapsHTMFileToolStripMenuItem");
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Text("Save GoogleMaps HTM file");
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).add_Click((EventHandler)saveGoogleMapsHTMFileToolStripMenuItem_Click);
			toolTip1.set_AutoPopDelay(2000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(100);
			((Control)label5).set_Anchor((AnchorStyles)1);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(347, 56));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(142, 13));
			((Control)label5).set_TabIndex(20);
			((Control)label5).set_Text("Double-click to re-centre map");
			((Control)label6).set_Anchor((AnchorStyles)1);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(52, 36));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(169, 15));
			((Control)label6).set_TabIndex(21);
			((Control)label6).set_Text("Nominal map boundaries");
			cmbSiteFiles.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(525, 1));
			cmbSiteFiles.set_MaxDropDownItems(20);
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(114, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(28);
			cmbSiteFiles.add_SelectedIndexChanged((EventHandler)cmbSiteFiles_SelectedIndexChanged);
			cmbNames.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			cmbNames.get_Items().AddRange(new object[4] { "never", "all", "many", "some" });
			((Control)cmbNames).set_Location(new Point(645, 1));
			cmbNames.set_MaxDropDownItems(20);
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(55, 21));
			((Control)cmbNames).set_TabIndex(29);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(476, 5));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(49, 13));
			((Control)label7).set_TabIndex(30);
			((Control)label7).set_Text("Plot sites");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(6, 55));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(162, 13));
			((Control)label8).set_TabIndex(32);
			((Control)label8).set_Text("Right-click for menu options");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(836, 602));
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)cmbNames);
			((Control)this).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)picLocalMap);
			((Control)this).get_Controls().Add((Control)(object)cmdRePlot);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnSouth);
			((Control)this).get_Controls().Add((Control)(object)updnNorth);
			((Control)this).get_Controls().Add((Control)(object)updnEast);
			((Control)this).get_Controls().Add((Control)(object)updnWest);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_DataBindings().Add(new Binding("WindowState", (object)Settings.Default, "SolarEclipseLocalMapWindow", true, (DataSourceUpdateMode)1));
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseLocalMap", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationEclipseLocalMap);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("SolarEclipseLocalMap");
			((Control)this).set_Text("Solar Eclipse    Local map");
			((Form)this).set_WindowState(Settings.Default.SolarEclipseLocalMapWindow);
			((Form)this).add_Load((EventHandler)SolarEclipseLocalMap_Load);
			((Form)this).add_FormClosing(new FormClosingEventHandler(SolarEclipseLocalMap_FormClosing));
			((Control)this).add_Resize((EventHandler)SolarEclipseLocalMap_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnSouth).EndInit();
			((ISupportInitialize)updnNorth).EndInit();
			((ISupportInitialize)updnEast).EndInit();
			((ISupportInitialize)updnWest).EndInit();
			((ISupportInitialize)picLocalMap).EndInit();
			((Control)contextMenuStrip1).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
