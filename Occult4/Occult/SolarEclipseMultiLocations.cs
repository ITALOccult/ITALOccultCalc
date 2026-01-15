using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class SolarEclipseMultiLocations : Form
	{
		private readonly string AppPath;

		private static bool FormCreated = false;

		private static bool UseFullSitePrecisionInOutput = false;

		private static bool Add5th = true;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem ComputeUsingToolStripMenuItem;

		private ToolStripComboBox cmbSites;

		private ListBox lstPredictions;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem include5thlineBreaksToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byNameToolStripMenuItem;

		private ToolStripMenuItem byLongitudeToolStripMenuItem;

		private ToolStripMenuItem byLatitudeToolStripMenuItem;

		private ToolStripMenuItem limitSiteCoordinatePrecisionInOutputToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem multisitePredictionToolStripMenuItem;

		private ToolStripMenuItem detailedMapToolStripMenuItem;

		private ToolStripMenuItem localPredictionsToolStripMenuItem;

		private ToolStripMenuItem pathCoordinatesToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public SolarEclipseMultiLocations()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void SolarEclipseMultiLocations_Load(object sender, EventArgs e)
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
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSites.get_Items().Add((object)Path.GetFileName(path));
			}
			if ((cmbSites.get_Items().get_Count() > 0) & (Settings.Default.MultiLocationSelectedIndex >= 0) & (Settings.Default.MultiLocationSelectedIndex < cmbSites.get_Items().get_Count()))
			{
				cmbSites.set_SelectedIndex(Settings.Default.MultiLocationSelectedIndex);
				((ToolStripControlHost)cmbSites).Focus();
			}
			FormCreated = true;
			SolarEclipseMultiLocations_Resize(sender, e);
		}

		private void SolarEclipseMultiLocations_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 100)))
			{
				((Control)lstPredictions).set_Width(((Control)this).get_Width() - 25);
				((Control)lstPredictions).set_Height(((Control)this).get_Height() - 90);
			}
		}

		public void ComputePrediction()
		{
			if (cmbSites.get_Items().get_Count() > 0)
			{
				SolarEclipses.MultiLocationComputation(cmbSites.get_Items().get_Item(cmbSites.get_SelectedIndex()).ToString(), UseFullSitePrecisionInOutput);
				EclipseContactTimes.SortField = 0;
				Display();
			}
		}

		private void byNameToolStripMenuItem_Click_1(object sender, EventArgs e)
		{
			EclipseContactTimes.SortField = 0;
			Display();
		}

		private void byLongitudeToolStripMenuItem_Click_1(object sender, EventArgs e)
		{
			EclipseContactTimes.SortField = 1;
			Display();
		}

		private void byLatitudeToolStripMenuItem_Click_1(object sender, EventArgs e)
		{
			EclipseContactTimes.SortField = 2;
			Display();
		}

		private void Display()
		{
			SolarEclipses.MultiEvent.Sort();
			lstPredictions.BeginUpdate();
			lstPredictions.get_Items().Clear();
			lstPredictions.get_Items().Add((object)(SolarEclipses.SolarEclipseLabel + " - multisite predictions"));
			lstPredictions.get_Items().Add((object)"");
			lstPredictions.get_Items().Add((object)"                                                              1st Contact         2nd Contact      Maximum        3rd Contact      4th Contact                 Mag  Central");
			lstPredictions.get_Items().Add((object)"Site                              Longitude Latitude  Elvn      U.T.     PA Alt     U.T.     PA      U.T.   Alt     U.T.     PA      U.T.     PA Alt   Mag    (dia   Durn");
			lstPredictions.get_Items().Add((object)"                                    o   '    o   '       m     h  m  s     o  o    h  m  s     o    h  m  s   o    h  m  s     o    h  m  s     o  o          ratio)  sec");
			for (int i = 0; i < SolarEclipses.MultiEvent.Count; i++)
			{
				lstPredictions.get_Items().Add((object)SolarEclipses.MultiEvent[i].ToString(!UseFullSitePrecisionInOutput));
				if (((i + 1) % 5 == 0) & Add5th)
				{
					lstPredictions.get_Items().Add((object)"");
				}
			}
			lstPredictions.EndUpdate();
			((Control)this).Focus();
		}

		private void cmbSites_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				Settings.Default.MultiLocationSelectedIndex = cmbSites.get_SelectedIndex();
				ComputePrediction();
			}
		}

		private void ComputeUsingToolStripMenuItem_Click(object sender, EventArgs e)
		{
			cmbSites_SelectedIndexChanged(sender, e);
		}

		private void include5thlineBreaksToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Add5th = !Add5th;
			include5thlineBreaksToolStripMenuItem.set_Checked(Add5th);
			Display();
		}

		private string CollectEvents()
		{
			if (lstPredictions.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstPredictions.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstPredictions.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_SolarEclipses = Output.SaveAppendPredictionText(CollectEvents(), SolarEclipses.SolarEclipseLabel + " - Multisites", Settings.Default.Save_SolarEclipses);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void limitSiteCoordinatePrecisionInOutputToolStripMenuItem_Click_1(object sender, EventArgs e)
		{
			UseFullSitePrecisionInOutput = !UseFullSitePrecisionInOutput;
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem.set_Checked(!UseFullSitePrecisionInOutput);
			ComputeUsingToolStripMenuItem_Click(sender, e);
		}

		private void detailedMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowLocalMap(UseCursorPosition: false);
		}

		private void localPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowSingleLocation();
		}

		private void pathCoordinatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_PathCoordinates();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solar eclipse - MultiLocation predictions");
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
			//IL_0a2b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a35: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			include5thlineBreaksToolStripMenuItem = new ToolStripMenuItem();
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			ComputeUsingToolStripMenuItem = new ToolStripMenuItem();
			cmbSites = new ToolStripComboBox();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byNameToolStripMenuItem = new ToolStripMenuItem();
			byLongitudeToolStripMenuItem = new ToolStripMenuItem();
			byLatitudeToolStripMenuItem = new ToolStripMenuItem();
			multisitePredictionToolStripMenuItem = new ToolStripMenuItem();
			detailedMapToolStripMenuItem = new ToolStripMenuItem();
			localPredictionsToolStripMenuItem = new ToolStripMenuItem();
			pathCoordinatesToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstPredictions = new ListBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)ComputeUsingToolStripMenuItem,
				(ToolStripItem)cmbSites,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)multisitePredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1094, 27));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)include5thlineBreaksToolStripMenuItem,
				(ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(120, 23));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text(" with Prediction...   ");
			include5thlineBreaksToolStripMenuItem.set_Checked(true);
			include5thlineBreaksToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Name("include5thlineBreaksToolStripMenuItem");
			include5thlineBreaksToolStripMenuItem.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Text("Include 5th-line breaks");
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).add_Click((EventHandler)include5thlineBreaksToolStripMenuItem_Click);
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem.set_Checked(true);
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).set_Name("limitSiteCoordinatePrecisionInOutputToolStripMenuItem");
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).set_Text("Limit site coordinate precision in output");
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).add_Click((EventHandler)limitSiteCoordinatePrecisionInOutputToolStripMenuItem_Click_1);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(282, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)ComputeUsingToolStripMenuItem).set_Name("ComputeUsingToolStripMenuItem");
			((ToolStripItem)ComputeUsingToolStripMenuItem).set_Size(new Size(120, 23));
			((ToolStripItem)ComputeUsingToolStripMenuItem).set_Text("Compute using =>");
			((ToolStripItem)ComputeUsingToolStripMenuItem).add_Click((EventHandler)ComputeUsingToolStripMenuItem_Click);
			cmbSites.set_DropDownStyle((ComboBoxStyle)2);
			cmbSites.set_MaxDropDownItems(20);
			((ToolStripItem)cmbSites).set_Name("cmbSites");
			((ToolStripItem)cmbSites).set_Size(new Size(150, 23));
			cmbSites.set_Sorted(true);
			cmbSites.add_SelectedIndexChanged((EventHandler)cmbSites_SelectedIndexChanged);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)byNameToolStripMenuItem,
				(ToolStripItem)byLongitudeToolStripMenuItem,
				(ToolStripItem)byLatitudeToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(49, 23));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...");
			((ToolStripItem)byNameToolStripMenuItem).set_Name("byNameToolStripMenuItem");
			((ToolStripItem)byNameToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byNameToolStripMenuItem).set_Text("by Name");
			((ToolStripItem)byNameToolStripMenuItem).add_Click((EventHandler)byNameToolStripMenuItem_Click_1);
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Name("byLongitudeToolStripMenuItem");
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Text("by Longitude");
			((ToolStripItem)byLongitudeToolStripMenuItem).add_Click((EventHandler)byLongitudeToolStripMenuItem_Click_1);
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Name("byLatitudeToolStripMenuItem");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Text("by Latitude");
			((ToolStripItem)byLatitudeToolStripMenuItem).add_Click((EventHandler)byLatitudeToolStripMenuItem_Click_1);
			((ToolStripDropDownItem)multisitePredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)detailedMapToolStripMenuItem,
				(ToolStripItem)localPredictionsToolStripMenuItem,
				(ToolStripItem)pathCoordinatesToolStripMenuItem
			});
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Name("multisitePredictionToolStripMenuItem");
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Size(new Size(133, 23));
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Text("Detailed predictions...");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Name("detailedMapToolStripMenuItem");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Size(new Size(164, 22));
			((ToolStripItem)detailedMapToolStripMenuItem).set_Text("Detailed map");
			((ToolStripItem)detailedMapToolStripMenuItem).add_Click((EventHandler)detailedMapToolStripMenuItem_Click);
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Name("localPredictionsToolStripMenuItem");
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Size(new Size(164, 22));
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Text("Local predictions");
			((ToolStripItem)localPredictionsToolStripMenuItem).add_Click((EventHandler)localPredictionsToolStripMenuItem_Click);
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Name("pathCoordinatesToolStripMenuItem");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Size(new Size(164, 22));
			((ToolStripItem)pathCoordinatesToolStripMenuItem).set_Text("Path coordinates");
			((ToolStripItem)pathCoordinatesToolStripMenuItem).add_Click((EventHandler)pathCoordinatesToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 23));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 23));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstPredictions).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPredictions).set_FormattingEnabled(true);
			lstPredictions.set_HorizontalExtent(1200);
			lstPredictions.set_HorizontalScrollbar(true);
			lstPredictions.set_ItemHeight(14);
			((Control)lstPredictions).set_Location(new Point(9, 42));
			((Control)lstPredictions).set_Name("lstPredictions");
			((Control)lstPredictions).set_Size(new Size(1158, 606));
			((Control)lstPredictions).set_TabIndex(1);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1094, 669));
			((Control)this).get_Controls().Add((Control)(object)lstPredictions);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseSolarMulti", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationEclipseSolarMulti);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("SolarEclipseMultiLocations");
			((Control)this).set_Text("Solar Eclipse   Multi-Location predictions");
			((Form)this).add_Load((EventHandler)SolarEclipseMultiLocations_Load);
			((Control)this).add_Resize((EventHandler)SolarEclipseMultiLocations_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
