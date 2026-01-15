using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class AsteroidMultiLocations : Form
	{
		private readonly string AppPath;

		private static bool FormCreated;

		private static bool Miles;

		private static bool UseFullSitePrecisionInOutput;

		private int SigmaLimit;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripComboBox cmbSites;

		private ListBox lstPredictions;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem omputeUsingToolStripMenuItem;

		private ToolStripMenuItem setDistanceLimitsToolStripMenuItem;

		private ToolStripMenuItem include5thlineBreaksToolStripMenuItem;

		private ToolStripMenuItem noLimitToolStripMenuItem;

		private ToolStripMenuItem limitOutputTo1sigmaToolStripMenuItem;

		private ToolStripMenuItem limitOutputTo3sigmaToolStripMenuItem;

		private ToolStripMenuItem limitOutputTo5sigmaToolStripMenuItem;

		private ToolStripMenuItem saveCurrentLimitAsDefaultToolStripMenuItem;

		private ToolStripMenuItem distancesInMilesnotKmToolStripMenuItem;

		private ToolStripMenuItem limitSiteCoordinatePrecisionInOutputToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byDistanceToolStripMenuItem;

		private ToolStripMenuItem byNameToolStripMenuItem;

		private ToolStripMenuItem byLongitudeToolStripMenuItem;

		private ToolStripMenuItem byLatitudeToolStripMenuItem;

		private ToolStripMenuItem byUTToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem probabilityUsing3sigmaToolStripMenuItem;

		public AsteroidMultiLocations()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void AsteroidMultiLocations_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSites.get_Items().Add((object)Path.GetFileName(path));
			}
			SigmaLimit = Settings.Default.AsteroidMultiSigma;
			setLimitChecks();
			Miles = Settings.Default.AsteroidMultiMiles;
			distancesInMilesnotKmToolStripMenuItem.set_Checked(Miles);
			probabilityUsing3sigmaToolStripMenuItem.set_Checked(Settings.Default.MultiPredict3Sigma);
			if ((cmbSites.get_Items().get_Count() > 0) & (Settings.Default.MultiLocationSelectedIndex >= 0) & (Settings.Default.MultiLocationSelectedIndex < cmbSites.get_Items().get_Count()))
			{
				cmbSites.set_SelectedIndex(Settings.Default.MultiLocationSelectedIndex);
			}
			include5thlineBreaksToolStripMenuItem.set_Checked(Settings.Default.Skip5thLine);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			FormCreated = true;
		}

		private void AsteroidMultiLocations_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 200)))
			{
				((Control)lstPredictions).set_Height(((Control)this).get_Height() - 88);
				((Control)lstPredictions).set_Width(((Control)this).get_Width() - 28);
			}
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
			ComputePrediction();
		}

		public void ComputePrediction()
		{
			if (cmbSites.get_Items().get_Count() > 0)
			{
				DisplayMPOccultations.MultiLocationComputation(cmbSites.get_Items().get_Item(cmbSites.get_SelectedIndex()).ToString(), SigmaLimit, Miles, probabilityUsing3sigmaToolStripMenuItem.get_Checked());
				AsteroidMultiPredictLine.SortField = 0;
				Display();
			}
		}

		private void include5thlineBreaksToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Skip5thLine = !Settings.Default.Skip5thLine;
			include5thlineBreaksToolStripMenuItem.set_Checked(Settings.Default.Skip5thLine);
			Display();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			if (lstPredictions.get_SelectedIndices().get_Count() <= 0)
			{
				return;
			}
			if (lstPredictions.get_SelectedIndices().get_Item(0) > 8)
			{
				for (int i = 0; i < 9; i++)
				{
					text = text + lstPredictions.get_Items().get_Item(i).ToString() + "\r\n";
				}
			}
			foreach (object selectedItem in lstPredictions.get_SelectedItems())
			{
				text = text + selectedItem.ToString() + "\r\n";
			}
			Clipboard.SetText(text);
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
			Settings.Default.Save_AsteroidPredictions = Output.SaveAppendPredictionText(CollectEvents(), DisplayMPOccultations.UTDate.Trim() + " " + DisplayMPOccultations.AsteroidName.Trim() + " Multilocations", Settings.Default.Save_AsteroidPredictions);
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

		private void limitOutputTo1sigmaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SigmaLimit = 1;
			setLimitChecks();
			ComputeUsingToolStripMenuItem_Click(sender, e);
		}

		private void limitOutputTo2sigmaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SigmaLimit = 2;
			setLimitChecks();
			ComputeUsingToolStripMenuItem_Click(sender, e);
		}

		private void limitOutputTo3sigmaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SigmaLimit = 3;
			setLimitChecks();
			ComputeUsingToolStripMenuItem_Click(sender, e);
		}

		private void noLimitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SigmaLimit = 0;
			setLimitChecks();
			ComputeUsingToolStripMenuItem_Click(sender, e);
		}

		private void setLimitChecks()
		{
			noLimitToolStripMenuItem.set_Checked(SigmaLimit == 0);
			limitOutputTo1sigmaToolStripMenuItem.set_Checked(SigmaLimit == 1);
			limitOutputTo3sigmaToolStripMenuItem.set_Checked(SigmaLimit == 2);
			limitOutputTo5sigmaToolStripMenuItem.set_Checked(SigmaLimit == 3);
		}

		private void saveCurrentLimitAsDefaultToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to save the current criteria as default values?", "Save Criteria", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				Settings.Default.AsteroidMultiMiles = Miles;
				Settings.Default.AsteroidMultiSigma = SigmaLimit;
			}
		}

		private void distancesInMilesnotKmToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Miles = !Miles;
			distancesInMilesnotKmToolStripMenuItem.set_Checked(Miles);
			ComputeUsingToolStripMenuItem_Click(sender, e);
		}

		private void limitSiteCoordinatePrecisionInOutputToolStripMenuItem_Click(object sender, EventArgs e)
		{
			UseFullSitePrecisionInOutput = !UseFullSitePrecisionInOutput;
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem.set_Checked(!UseFullSitePrecisionInOutput);
			ComputeUsingToolStripMenuItem_Click(sender, e);
		}

		private void byDistanceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidMultiPredictLine.SortField = 0;
			Display();
		}

		private void byNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidMultiPredictLine.SortField = 1;
			Display();
		}

		private void byLongitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidMultiPredictLine.SortField = 2;
			Display();
		}

		private void byLatitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidMultiPredictLine.SortField = 3;
			Display();
		}

		private void byUTToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AsteroidMultiPredictLine.SortField = 4;
			Display();
		}

		private void Display()
		{
			DisplayMPOccultations.MultiEvent.Sort();
			lstPredictions.BeginUpdate();
			lstPredictions.get_Items().Clear();
			bool flag = false;
			bool flag2 = false;
			int num = 1;
			if (probabilityUsing3sigmaToolStripMenuItem.get_Checked())
			{
				num = 3;
			}
			lstPredictions.get_Items().Add((object)DisplayMPOccultations.EventHeader);
			lstPredictions.get_Items().Add((object)"");
			if (Miles)
			{
				lstPredictions.get_Items().Add((object)"Distance from center of occultation path - in miles");
			}
			else
			{
				lstPredictions.get_Items().Add((object)"Distance from center of occultation path - in km");
			}
			lstPredictions.get_Items().Add((object)"Distances are positive to the right, referenced to the direction of motion along the path");
			lstPredictions.get_Items().Add((object)("Probability based on " + num + "-sigma uncertainty"));
			lstPredictions.get_Items().Add((object)("Uncertainty in time = +/- " + string.Format("{0,1:F0} secs", DisplayMPOccultations.ErrorInTime * (double)num)));
			lstPredictions.get_Items().Add((object)"");
			lstPredictions.get_Items().Add((object)"          d Proba-  Location                           Longitude   Latitude    alt      U.T.     Sepn   Alt  Sun");
			if (Miles)
			{
				lstPredictions.get_Items().Add((object)"       mi   bility                                       o  '  \"  o  '  \"    m    h  m  s     \"       o    o");
			}
			else
			{
				lstPredictions.get_Items().Add((object)"       km   bility                                       o  '  \"    o  '  \"      m    h  m  s     \"       o    o");
			}
			for (int i = 0; i < DisplayMPOccultations.MultiEvent.Count; i++)
			{
				string text = DisplayMPOccultations.MultiEvent[i].ToString(!UseFullSitePrecisionInOutput);
				lstPredictions.get_Items().Add((object)text);
				if (text.Length > 85)
				{
					if (text.Substring(76, 8).Contains("-"))
					{
						flag = true;
					}
					if (text.Substring(76, 8).Contains("+"))
					{
						flag2 = true;
					}
				}
				if (((i + 1) % 5 == 0) & Settings.Default.Skip5thLine)
				{
					lstPredictions.get_Items().Add((object)"");
				}
			}
			if (flag || flag2)
			{
				lstPredictions.get_Items().Add((object)"");
				if (flag)
				{
					lstPredictions.get_Items().Add((object)"UT times in the preceding day are preceded with '-'");
				}
				if (flag2)
				{
					lstPredictions.get_Items().Add((object)"UT times in the following day are preceded with '+'");
				}
			}
			lstPredictions.get_Items().Add((object)"");
			lstPredictions.get_Items().Add((object)DisplayMPOccultations.PredictionDate);
			lstPredictions.EndUpdate();
			((Control)this).Focus();
		}

		private void probabilityUsing3sigmaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.MultiPredict3Sigma = probabilityUsing3sigmaToolStripMenuItem.get_Checked();
			ComputePrediction();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid Multilocation predictions");
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
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
			//IL_0da1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dab: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidMultiLocations));
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			include5thlineBreaksToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			probabilityUsing3sigmaToolStripMenuItem = new ToolStripMenuItem();
			omputeUsingToolStripMenuItem = new ToolStripMenuItem();
			cmbSites = new ToolStripComboBox();
			setDistanceLimitsToolStripMenuItem = new ToolStripMenuItem();
			limitOutputTo1sigmaToolStripMenuItem = new ToolStripMenuItem();
			limitOutputTo3sigmaToolStripMenuItem = new ToolStripMenuItem();
			limitOutputTo5sigmaToolStripMenuItem = new ToolStripMenuItem();
			noLimitToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			distancesInMilesnotKmToolStripMenuItem = new ToolStripMenuItem();
			saveCurrentLimitAsDefaultToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byDistanceToolStripMenuItem = new ToolStripMenuItem();
			byNameToolStripMenuItem = new ToolStripMenuItem();
			byLongitudeToolStripMenuItem = new ToolStripMenuItem();
			byLatitudeToolStripMenuItem = new ToolStripMenuItem();
			byUTToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstPredictions = new ListBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)omputeUsingToolStripMenuItem,
				(ToolStripItem)cmbSites,
				(ToolStripItem)setDistanceLimitsToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(841, 27));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)include5thlineBreaksToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)probabilityUsing3sigmaToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(120, 23));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...    ");
			include5thlineBreaksToolStripMenuItem.set_Checked(true);
			include5thlineBreaksToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Name("include5thlineBreaksToolStripMenuItem");
			include5thlineBreaksToolStripMenuItem.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Text("Include 5th-line breaks");
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).add_Click((EventHandler)include5thlineBreaksToolStripMenuItem_Click);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copySelectedToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy All");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			probabilityUsing3sigmaToolStripMenuItem.set_Checked(Settings.Default.MultiPredict3Sigma);
			probabilityUsing3sigmaToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)probabilityUsing3sigmaToolStripMenuItem).set_Image((Image)Resources.Question_16x_24);
			((ToolStripItem)probabilityUsing3sigmaToolStripMenuItem).set_Name("probabilityUsing3sigmaToolStripMenuItem");
			((ToolStripItem)probabilityUsing3sigmaToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)probabilityUsing3sigmaToolStripMenuItem).set_Text("Probability based on 3-sigma");
			((ToolStripItem)probabilityUsing3sigmaToolStripMenuItem).add_Click((EventHandler)probabilityUsing3sigmaToolStripMenuItem_Click);
			((ToolStripItem)omputeUsingToolStripMenuItem).set_Name("omputeUsingToolStripMenuItem");
			((ToolStripItem)omputeUsingToolStripMenuItem).set_Size(new Size(120, 23));
			((ToolStripItem)omputeUsingToolStripMenuItem).set_Text("&Compute using =>");
			((ToolStripItem)omputeUsingToolStripMenuItem).add_Click((EventHandler)ComputeUsingToolStripMenuItem_Click);
			cmbSites.set_DropDownStyle((ComboBoxStyle)2);
			cmbSites.set_MaxDropDownItems(20);
			((ToolStripItem)cmbSites).set_Name("cmbSites");
			((ToolStripItem)cmbSites).set_Size(new Size(150, 23));
			cmbSites.set_Sorted(true);
			cmbSites.add_SelectedIndexChanged((EventHandler)cmbSites_SelectedIndexChanged);
			((ToolStripDropDownItem)setDistanceLimitsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)limitOutputTo1sigmaToolStripMenuItem,
				(ToolStripItem)limitOutputTo3sigmaToolStripMenuItem,
				(ToolStripItem)limitOutputTo5sigmaToolStripMenuItem,
				(ToolStripItem)noLimitToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)distancesInMilesnotKmToolStripMenuItem,
				(ToolStripItem)saveCurrentLimitAsDefaultToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem
			});
			((ToolStripItem)setDistanceLimitsToolStripMenuItem).set_Name("setDistanceLimitsToolStripMenuItem");
			((ToolStripItem)setDistanceLimitsToolStripMenuItem).set_Size(new Size(135, 23));
			((ToolStripItem)setDistanceLimitsToolStripMenuItem).set_Text("    Set distance limits...");
			((ToolStripItem)limitOutputTo1sigmaToolStripMenuItem).set_Name("limitOutputTo1sigmaToolStripMenuItem");
			((ToolStripItem)limitOutputTo1sigmaToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)limitOutputTo1sigmaToolStripMenuItem).set_Text("Limit output to 1-sigma");
			((ToolStripItem)limitOutputTo1sigmaToolStripMenuItem).add_Click((EventHandler)limitOutputTo1sigmaToolStripMenuItem_Click);
			((ToolStripItem)limitOutputTo3sigmaToolStripMenuItem).set_Name("limitOutputTo3sigmaToolStripMenuItem");
			((ToolStripItem)limitOutputTo3sigmaToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)limitOutputTo3sigmaToolStripMenuItem).set_Text("Limit output to 2-sigma");
			((ToolStripItem)limitOutputTo3sigmaToolStripMenuItem).add_Click((EventHandler)limitOutputTo2sigmaToolStripMenuItem_Click);
			((ToolStripItem)limitOutputTo5sigmaToolStripMenuItem).set_Name("limitOutputTo5sigmaToolStripMenuItem");
			((ToolStripItem)limitOutputTo5sigmaToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)limitOutputTo5sigmaToolStripMenuItem).set_Text("Limit output to 3-sigma");
			((ToolStripItem)limitOutputTo5sigmaToolStripMenuItem).add_Click((EventHandler)limitOutputTo3sigmaToolStripMenuItem_Click);
			((ToolStripItem)noLimitToolStripMenuItem).set_Name("noLimitToolStripMenuItem");
			((ToolStripItem)noLimitToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)noLimitToolStripMenuItem).set_Text("No limit - list all sites");
			((ToolStripItem)noLimitToolStripMenuItem).add_Click((EventHandler)noLimitToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(282, 6));
			((ToolStripItem)distancesInMilesnotKmToolStripMenuItem).set_Name("distancesInMilesnotKmToolStripMenuItem");
			((ToolStripItem)distancesInMilesnotKmToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)distancesInMilesnotKmToolStripMenuItem).set_Text("Distances in Miles (not km)");
			((ToolStripItem)distancesInMilesnotKmToolStripMenuItem).add_Click((EventHandler)distancesInMilesnotKmToolStripMenuItem_Click);
			((ToolStripItem)saveCurrentLimitAsDefaultToolStripMenuItem).set_Name("saveCurrentLimitAsDefaultToolStripMenuItem");
			((ToolStripItem)saveCurrentLimitAsDefaultToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)saveCurrentLimitAsDefaultToolStripMenuItem).set_Text("Save limit values as default");
			((ToolStripItem)saveCurrentLimitAsDefaultToolStripMenuItem).add_Click((EventHandler)saveCurrentLimitAsDefaultToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(282, 6));
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem.set_Checked(true);
			limitSiteCoordinatePrecisionInOutputToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).set_Name("limitSiteCoordinatePrecisionInOutputToolStripMenuItem");
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).set_Size(new Size(285, 22));
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).set_Text("Limit site coordinate precision in output");
			((ToolStripItem)limitSiteCoordinatePrecisionInOutputToolStripMenuItem).add_Click((EventHandler)limitSiteCoordinatePrecisionInOutputToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)byDistanceToolStripMenuItem,
				(ToolStripItem)byNameToolStripMenuItem,
				(ToolStripItem)byLongitudeToolStripMenuItem,
				(ToolStripItem)byLatitudeToolStripMenuItem,
				(ToolStripItem)byUTToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(65, 23));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...");
			((ToolStripItem)byDistanceToolStripMenuItem).set_Name("byDistanceToolStripMenuItem");
			((ToolStripItem)byDistanceToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byDistanceToolStripMenuItem).set_Text("by Distance");
			((ToolStripItem)byDistanceToolStripMenuItem).add_Click((EventHandler)byDistanceToolStripMenuItem_Click);
			((ToolStripItem)byNameToolStripMenuItem).set_Name("byNameToolStripMenuItem");
			((ToolStripItem)byNameToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byNameToolStripMenuItem).set_Text("by Name");
			((ToolStripItem)byNameToolStripMenuItem).add_Click((EventHandler)byNameToolStripMenuItem_Click);
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Name("byLongitudeToolStripMenuItem");
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Text("by Longitude");
			((ToolStripItem)byLongitudeToolStripMenuItem).add_Click((EventHandler)byLongitudeToolStripMenuItem_Click);
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Name("byLatitudeToolStripMenuItem");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Text("by Latitude");
			((ToolStripItem)byLatitudeToolStripMenuItem).add_Click((EventHandler)byLatitudeToolStripMenuItem_Click);
			((ToolStripItem)byUTToolStripMenuItem).set_Name("byUTToolStripMenuItem");
			((ToolStripItem)byUTToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byUTToolStripMenuItem).set_Text("by UT");
			((ToolStripItem)byUTToolStripMenuItem).add_Click((EventHandler)byUTToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 23));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 23));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstPredictions).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPredictions).set_FormattingEnabled(true);
			lstPredictions.set_HorizontalExtent(720);
			lstPredictions.set_HorizontalScrollbar(true);
			lstPredictions.set_ItemHeight(14);
			((Control)lstPredictions).set_Location(new Point(10, 44));
			((Control)lstPredictions).set_Name("lstPredictions");
			lstPredictions.set_SelectionMode((SelectionMode)3);
			((Control)lstPredictions).set_Size(new Size(823, 466));
			((Control)lstPredictions).set_TabIndex(1);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(841, 523));
			((Control)this).get_Controls().Add((Control)(object)lstPredictions);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterMultiLocn", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterMultiLocn);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidMultiLocations");
			((Control)this).set_Text("Asteroid - Multi-location predictions");
			((Form)this).add_Load((EventHandler)AsteroidMultiLocations_Load);
			((Control)this).add_Resize((EventHandler)AsteroidMultiLocations_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
