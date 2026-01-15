using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class SolarEclipseLocalPrediction : Form
	{
		private readonly string AppPath;

		private static string Prediction;

		private IContainer components;

		private TextBox txtPrediction;

		private MenuStrip menuStrip1;

		private Button cmdCompute;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem multisitePredictionToolStripMenuItem;

		private ToolStripMenuItem detailedMapToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem multiLocationPredictionsToolStripMenuItem;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem;

		internal TextBox txtLongitudeDeg;

		internal TextBox txtLatitudeMin;

		internal TextBox txtLatitudeDeg;

		internal TextBox txtLongitudeMin;

		internal TextBox txtAltitude;

		internal TextBox txtSite;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem litBesselianElementsToolStripMenuItem;

		private ToolStripMenuItem lunarProfileGenerateSaveToolStripMenuItem;

		internal CheckBox chkApplyLimbCorrections;

		public SolarEclipseLocalPrediction()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void SolarEclipseLocalPrediction_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)chkApplyLimbCorrections).set_Enabled(Utilities.LOLAFileExists);
		}

		public void ComputePrediction()
		{
			GetSiteCoords(out var Alt, out var Longitude, out var Latitude);
			Prediction = SolarEclipses.LocalCircumstances(((Control)txtSite).get_Text().Trim(), Longitude, Latitude, Alt, ((Control)chkApplyLimbCorrections).get_Enabled() & chkApplyLimbCorrections.get_Checked() & SolarEclipses.UseUTC);
			((Control)txtPrediction).set_Text(Prediction);
			Application.DoEvents();
			try
			{
				SolarEclipseLocalMap.MouseMoveDisabled = false;
			}
			catch
			{
			}
			try
			{
				SolarEclipseWorldView.MouseMoveDisabled = false;
			}
			catch
			{
			}
		}

		private void GetSiteCoords(out double Alt, out double Longitude, out double Latitude)
		{
			if (!double.TryParse(((Control)txtLongitudeDeg).get_Text().Replace(" ", "0"), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtLongitudeMin).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(((Control)txtLatitudeDeg).get_Text().Replace(" ", "0"), out var result3))
			{
				result3 = 0.0;
			}
			if (!double.TryParse(((Control)txtLatitudeMin).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			if (!double.TryParse(((Control)txtAltitude).get_Text(), out Alt))
			{
				Alt = 0.0;
			}
			Longitude = Math.Abs(result) + result2 / 60.0;
			if (((Control)txtLongitudeDeg).get_Text().IndexOf("-") > -1)
			{
				Longitude = 0.0 - Longitude;
			}
			Latitude = Math.Abs(result3) + result4 / 60.0;
			if (((Control)txtLatitudeDeg).get_Text().IndexOf("-") > -1)
			{
				Latitude = 0.0 - Latitude;
			}
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			ComputePrediction();
		}

		private void txtLongitudeDeg_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongitudeDeg).SelectAll();
		}

		private void txtLongitudeMin_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongitudeMin).SelectAll();
		}

		private void txtLatitudeDeg_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatitudeDeg).SelectAll();
		}

		private void txtLatitudeMin_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatitudeMin).SelectAll();
		}

		private void txtAltitude_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAltitude).SelectAll();
		}

		private void txtSite_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSite).SelectAll();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(Prediction);
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(Prediction);
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(Prediction);
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_SolarEclipses = Output.SaveAppendPredictionText(Prediction, SolarEclipses.SolarEclipseLabel + " at " + ((Control)txtSite).get_Text().Trim(), Settings.Default.Save_SolarEclipses);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void detailedMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowLocalMap(UseCursorPosition: false);
		}

		private void multiLocationPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_Multilocations();
		}

		private void pathCoordinatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_PathCoordinates();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solar eclipse - local prediction");
		}

		private void listBesselianElementsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ListBesselianElements();
		}

		private void lunarProfileGenerateSaveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetSiteCoords(out var _, out var Longitude, out var Latitude);
			SolarEclipses.GenerateLunarProfile(Longitude, Latitude, SaveFile: true);
		}

		private void chkApplyLimbCorrections_CheckedChanged(object sender, EventArgs e)
		{
			ComputePrediction();
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
			//IL_0f13: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f1d: Expected O, but got Unknown
			//IL_1126: Unknown result type (might be due to invalid IL or missing references)
			//IL_1130: Expected O, but got Unknown
			txtPrediction = new TextBox();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			litBesselianElementsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			multisitePredictionToolStripMenuItem = new ToolStripMenuItem();
			detailedMapToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			multiLocationPredictionsToolStripMenuItem = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem = new ToolStripMenuItem();
			lunarProfileGenerateSaveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdCompute = new Button();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			txtSite = new TextBox();
			txtAltitude = new TextBox();
			txtLongitudeMin = new TextBox();
			txtLatitudeDeg = new TextBox();
			txtLatitudeMin = new TextBox();
			txtLongitudeDeg = new TextBox();
			chkApplyLimbCorrections = new CheckBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)txtPrediction).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtPrediction).set_Location(new Point(6, 91));
			((TextBoxBase)txtPrediction).set_Multiline(true);
			((Control)txtPrediction).set_Name("txtPrediction");
			((TextBoxBase)txtPrediction).set_ReadOnly(true);
			((Control)txtPrediction).set_Size(new Size(613, 542));
			((Control)txtPrediction).set_TabIndex(17);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)multisitePredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(625, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)litBesselianElementsToolStripMenuItem,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...");
			((ToolStripItem)litBesselianElementsToolStripMenuItem).set_Image((Image)Resources.ShowGridlines2HS);
			((ToolStripItem)litBesselianElementsToolStripMenuItem).set_Name("litBesselianElementsToolStripMenuItem");
			((ToolStripItem)litBesselianElementsToolStripMenuItem).set_Size(new Size(194, 22));
			((ToolStripItem)litBesselianElementsToolStripMenuItem).set_Text("List Besselian elements");
			((ToolStripItem)litBesselianElementsToolStripMenuItem).add_Click((EventHandler)listBesselianElementsToolStripMenuItem_Click);
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
			((ToolStripDropDownItem)multisitePredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)detailedMapToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)multiLocationPredictionsToolStripMenuItem,
				(ToolStripItem)pathCoordinatesToolStripMenuItem,
				(ToolStripItem)lunarProfileGenerateSaveToolStripMenuItem
			});
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Name("multisitePredictionToolStripMenuItem");
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Size(new Size(133, 20));
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Text("Detailed predictions...");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Name("detailedMapToolStripMenuItem");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Size(new Size(237, 22));
			((ToolStripItem)detailedMapToolStripMenuItem).set_Text("Detailed map");
			((ToolStripItem)detailedMapToolStripMenuItem).add_Click((EventHandler)detailedMapToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(234, 6));
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Name("multiLocationPredictionsToolStripMenuItem");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Size(new Size(237, 22));
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Text("MultiLocation predictions");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).add_Click((EventHandler)multiLocationPredictionsToolStripMenuItem_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Name("pathCoordinatesToolStripMenuItem");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Size(new Size(237, 22));
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).add_Click((EventHandler)pathCoordinatesToolStripMenuItem_Click);
			((ToolStripItem)lunarProfileGenerateSaveToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)lunarProfileGenerateSaveToolStripMenuItem).set_Name("lunarProfileGenerateSaveToolStripMenuItem");
			((ToolStripItem)lunarProfileGenerateSaveToolStripMenuItem).set_Size(new Size(237, 22));
			((ToolStripItem)lunarProfileGenerateSaveToolStripMenuItem).set_Text("Lunar profile - generate && save");
			((ToolStripItem)lunarProfileGenerateSaveToolStripMenuItem).add_Click((EventHandler)lunarProfileGenerateSaveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdCompute).set_Location(new Point(536, 38));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(71, 42));
			((Control)cmdCompute).set_TabIndex(16);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(84, 26));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(13, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("o");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(118, 30));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(9, 13));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("'");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(210, 26));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(13, 13));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("o");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(245, 30));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(9, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("'");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(341, 28));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(15, 13));
			((Control)label5).set_TabIndex(13);
			((Control)label5).set_Text("m");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(6, 45));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(54, 13));
			((Control)label6).set_TabIndex(1);
			((Control)label6).set_Text("Longitude");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(152, 45));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(45, 13));
			((Control)label7).set_TabIndex(6);
			((Control)label7).set_Text("Latitude");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(276, 45));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(42, 13));
			((Control)label8).set_TabIndex(11);
			((Control)label8).set_Text("Altitude");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(6, 70));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(100, 13));
			((Control)label9).set_TabIndex(15);
			((Control)label9).set_Text("Site name (optional)");
			((Control)txtSite).set_Location(new Point(110, 66));
			((Control)txtSite).set_Name("txtSite");
			((Control)txtSite).set_Size(new Size(171, 20));
			((Control)txtSite).set_TabIndex(14);
			txtSite.set_TextAlign((HorizontalAlignment)2);
			((Control)txtSite).add_Enter((EventHandler)txtSite_Enter);
			((Control)txtAltitude).set_Location(new Point(318, 41));
			((Control)txtAltitude).set_Name("txtAltitude");
			((Control)txtAltitude).set_Size(new Size(38, 20));
			((Control)txtAltitude).set_TabIndex(12);
			((Control)txtAltitude).set_Text("0");
			txtAltitude.set_TextAlign((HorizontalAlignment)1);
			((Control)txtAltitude).add_Enter((EventHandler)txtAltitude_Enter);
			((Control)txtLongitudeMin).set_Location(new Point(104, 41));
			((TextBoxBase)txtLongitudeMin).set_MaxLength(5);
			((Control)txtLongitudeMin).set_Name("txtLongitudeMin");
			((Control)txtLongitudeMin).set_Size(new Size(31, 20));
			((Control)txtLongitudeMin).set_TabIndex(4);
			((Control)txtLongitudeMin).set_Text("0");
			txtLongitudeMin.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongitudeMin).add_Enter((EventHandler)txtLongitudeMin_Enter);
			((Control)txtLatitudeDeg).set_Location(new Point(197, 41));
			((TextBoxBase)txtLatitudeDeg).set_MaxLength(4);
			((Control)txtLatitudeDeg).set_Name("txtLatitudeDeg");
			((Control)txtLatitudeDeg).set_Size(new Size(25, 20));
			((Control)txtLatitudeDeg).set_TabIndex(7);
			((Control)txtLatitudeDeg).set_Text("0");
			txtLatitudeDeg.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLatitudeDeg).add_Enter((EventHandler)txtLatitudeDeg_Enter);
			((Control)txtLatitudeMin).set_Location(new Point(230, 41));
			((TextBoxBase)txtLatitudeMin).set_MaxLength(5);
			((Control)txtLatitudeMin).set_Name("txtLatitudeMin");
			((Control)txtLatitudeMin).set_Size(new Size(31, 20));
			((Control)txtLatitudeMin).set_TabIndex(9);
			((Control)txtLatitudeMin).set_Text("0");
			txtLatitudeMin.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLatitudeMin).add_Enter((EventHandler)txtLatitudeMin_Enter);
			((Control)txtLongitudeDeg).set_Location(new Point(60, 41));
			((TextBoxBase)txtLongitudeDeg).set_MaxLength(4);
			((Control)txtLongitudeDeg).set_Name("txtLongitudeDeg");
			((Control)txtLongitudeDeg).set_Size(new Size(36, 20));
			((Control)txtLongitudeDeg).set_TabIndex(2);
			((Control)txtLongitudeDeg).set_Text("0");
			txtLongitudeDeg.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongitudeDeg).add_Enter((EventHandler)txtLongitudeDeg_Enter);
			((Control)chkApplyLimbCorrections).set_AutoSize(true);
			chkApplyLimbCorrections.set_Checked(Settings.Default.SolarEclipseLimbCorrections);
			chkApplyLimbCorrections.set_CheckState((CheckState)1);
			((Control)chkApplyLimbCorrections).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "SolarEclipseLimbCorrections", true, (DataSourceUpdateMode)1));
			((Control)chkApplyLimbCorrections).set_Location(new Point(375, 36));
			((Control)chkApplyLimbCorrections).set_Name("chkApplyLimbCorrections");
			((Control)chkApplyLimbCorrections).set_Size(new Size(137, 30));
			((Control)chkApplyLimbCorrections).set_TabIndex(18);
			((Control)chkApplyLimbCorrections).set_Text("Apply limb corrections &&\r\ninclude BailyBead times");
			((ButtonBase)chkApplyLimbCorrections).set_UseVisualStyleBackColor(true);
			chkApplyLimbCorrections.add_CheckedChanged((EventHandler)chkApplyLimbCorrections_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(625, 635));
			((Control)this).get_Controls().Add((Control)(object)chkApplyLimbCorrections);
			((Control)this).get_Controls().Add((Control)(object)txtSite);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)txtAltitude);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtLongitudeMin);
			((Control)this).get_Controls().Add((Control)(object)txtLatitudeDeg);
			((Control)this).get_Controls().Add((Control)(object)txtLatitudeMin);
			((Control)this).get_Controls().Add((Control)(object)txtLongitudeDeg);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)txtPrediction);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseLocalPredict", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationEclipseLocalPredict);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("SolarEclipseLocalPrediction");
			((Control)this).set_Text("Solar Eclipse    Local prediction");
			((Form)this).add_Load((EventHandler)SolarEclipseLocalPrediction_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
