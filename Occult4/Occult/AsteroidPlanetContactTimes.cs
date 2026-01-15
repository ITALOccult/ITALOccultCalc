using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class AsteroidPlanetContactTimes : Form
	{
		private readonly string AppPath;

		private bool FormCreated;

		private bool UseFullSitePrecisionInOutput;

		internal bool IsAsteroid;

		internal bool AsteroidHasSatellites;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem computeUsingToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem bySiteNameToolStripMenuItem;

		private ToolStripMenuItem byLongitudeToolStripMenuItem;

		private ToolStripMenuItem byLatitudeToolStripMenuItem;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem thLineBreaksToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstPredictions;

		private ToolStripMenuItem limitSitePrecisionInOutputToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem helpToolStripMenuItem;

		internal ToolStripComboBox cmbSites;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		public AsteroidPlanetContactTimes()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void AsteroidPlanetContactTimes_Load(object sender, EventArgs e)
		{
			thLineBreaksToolStripMenuItem.set_Checked(Settings.Default.Skip5thLine);
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
			if (cmbSites.get_Items().get_Count() > 0)
			{
				cmbSites.set_SelectedIndex(0);
				if ((cmbSites.get_Items().get_Count() > 0) & (Settings.Default.MultiLocationSelectedIndex >= 0) & (Settings.Default.MultiLocationSelectedIndex < cmbSites.get_Items().get_Count()))
				{
					cmbSites.set_SelectedIndex(Settings.Default.MultiLocationSelectedIndex);
				}
				((ToolStripControlHost)cmbSites).Focus();
				DisplayMPOccultations.MultiLocation_PlanetTimes(cmbSites.get_Items().get_Item(cmbSites.get_SelectedIndex()).ToString());
				PlanetContactTimes.SortField = 0;
				Display();
			}
			FormCreated = true;
			((Control)this).Focus();
		}

		private void AsteroidPlanetContactTimes_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 150)))
			{
				((Control)lstPredictions).set_Width(((Control)this).get_Width() - 32);
				((Control)lstPredictions).set_Height(((Control)this).get_Height() - 74);
			}
		}

		private void cmbSites_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				Settings.Default.MultiLocationSelectedIndex = cmbSites.get_SelectedIndex();
				computeUsingToolStripMenuItem_Click(sender, e);
			}
		}

		private void computeUsingToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultiLocation_PlanetTimes(cmbSites.get_Items().get_Item(cmbSites.get_SelectedIndex()).ToString());
			PlanetContactTimes.SortField = 0;
			Display();
		}

		private void bySiteNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlanetContactTimes.SortField = 0;
			Display();
		}

		private void byLongitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlanetContactTimes.SortField = 1;
			Display();
		}

		private void byLatitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlanetContactTimes.SortField = 2;
			Display();
		}

		private string FormatRingName(string Name)
		{
			int num = 9 - Name.Length;
			return ("".PadLeft(num / 2) + Name).PadRight(9);
		}

		internal void Display()
		{
			DisplayMPOccultations.MultiPlanetEvent.Sort();
			lstPredictions.BeginUpdate();
			lstPredictions.get_Items().Clear();
			lstPredictions.get_Items().Add((object)("Occultation of " + DisplayMPOccultations.StarNo.Trim() + " by " + DisplayMPOccultations.AsteroidName.Trim() + " on " + DisplayMPOccultations.UTDate));
			if (DisplayMPOccultations.AsteroidName.Contains("M00)") && Satellites.PlanetDiameterKM < 1000000f)
			{
				lstPredictions.get_Items().Add((object)"");
				lstPredictions.get_Items().Add((object)"Local predictions are not generated if the object's diameter is less than 1000 km");
			}
			else
			{
				lstPredictions.get_Items().Add((object)"");
				string text = "Location".PadRight(36) + "Long.   Latit. ";
				if (IsAsteroid)
				{
					int numOfRings = DisplayMPOccultations.NumOfRings;
					text += " T-closest   Alt ";
					for (int i = 1; i <= numOfRings; i++)
					{
						text = text + "  Ring " + i + " ";
					}
					for (int num = numOfRings; num >= 1; num--)
					{
						text = text + "  Ring " + num + " ";
					}
					lstPredictions.get_Items().Add((object)text);
					text = "".PadRight(36) + "o   '    o   '    h   m           ";
					for (int j = 1; j <= 2 * numOfRings; j++)
					{
						text += " h   m   ";
					}
					lstPredictions.get_Items().Add((object)text);
					for (int k = 0; k < DisplayMPOccultations.MultiPlanetEvent.Count; k++)
					{
						lstPredictions.get_Items().Add((object)DisplayMPOccultations.MultiPlanetEvent[k].ToString(!UseFullSitePrecisionInOutput));
						if (((k + 1) % 5 == 0) & Settings.Default.Skip5thLine)
						{
							lstPredictions.get_Items().Add((object)"");
						}
					}
				}
				else
				{
					for (int num2 = Satellites.NumberOfRings; num2 >= 1; num2--)
					{
						text += FormatRingName(Satellites.RingName[num2]);
					}
					text += "     D        R   ";
					for (int l = 1; l <= Satellites.NumberOfRings; l++)
					{
						text += FormatRingName(Satellites.RingName[l]);
					}
					lstPredictions.get_Items().Add((object)text);
					text = "".PadRight(36) + "o   '    o   '   h   m    h   m   ";
					for (int m = 1; m <= 2 * Satellites.NumberOfRings; m++)
					{
						text += " h   m   ";
					}
					lstPredictions.get_Items().Add((object)text);
					for (int n = 0; n < DisplayMPOccultations.MultiPlanetEvent.Count; n++)
					{
						lstPredictions.get_Items().Add((object)DisplayMPOccultations.MultiPlanetEvent[n].ToString(!UseFullSitePrecisionInOutput));
						if (((n + 1) % 5 == 0) & Settings.Default.Skip5thLine)
						{
							lstPredictions.get_Items().Add((object)"");
						}
					}
				}
			}
			lstPredictions.EndUpdate();
			((Control)this).Focus();
		}

		private void thLineBreaksToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Skip5thLine = !Settings.Default.Skip5thLine;
			thLineBreaksToolStripMenuItem.set_Checked(Settings.Default.Skip5thLine);
			Display();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPredictions.get_Items().get_Count() >= 4)
			{
				Clipboard.SetText(CollectEvents(SelectedOnly: false));
			}
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPredictions.get_Items().get_Count() >= 4)
			{
				Clipboard.SetText(CollectEvents(SelectedOnly: true));
			}
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPredictions.get_Items().get_Count() >= 4)
			{
				Output.PrintPreviewText(CollectEvents(SelectedOnly: false));
			}
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPredictions.get_Items().get_Count() >= 4)
			{
				Output.PrintText(CollectEvents(SelectedOnly: false));
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstPredictions.get_Items().get_Count() >= 4)
			{
				Settings.Default.Save_AsteroidPredictions = Output.SaveAppendPredictionText(CollectEvents(SelectedOnly: false), lstPredictions.get_Items().get_Item(0).ToString()!.Trim(), Settings.Default.Save_AsteroidPredictions);
			}
		}

		private string CollectEvents(bool SelectedOnly)
		{
			if (lstPredictions.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstPredictions.get_Items().get_Count();
			if (SelectedOnly)
			{
				for (int i = 0; i < 4; i++)
				{
					lstPredictions.SetSelected(i, true);
				}
				foreach (object selectedItem in lstPredictions.get_SelectedItems())
				{
					stringBuilder.AppendLine(selectedItem.ToString());
				}
			}
			else
			{
				for (int j = 0; j < count; j++)
				{
					stringBuilder.AppendLine(lstPredictions.get_Items().get_Item(j).ToString());
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void limitSitePrecisionInOutputToolStripMenuItem_Click(object sender, EventArgs e)
		{
			UseFullSitePrecisionInOutput = !UseFullSitePrecisionInOutput;
			limitSitePrecisionInOutputToolStripMenuItem.set_Checked(!UseFullSitePrecisionInOutput);
			computeUsingToolStripMenuItem_Click(sender, e);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Planet - contact times");
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
			//IL_09d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e1: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			thLineBreaksToolStripMenuItem = new ToolStripMenuItem();
			limitSitePrecisionInOutputToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			computeUsingToolStripMenuItem = new ToolStripMenuItem();
			cmbSites = new ToolStripComboBox();
			lstPredictions = new ListBox();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			bySiteNameToolStripMenuItem = new ToolStripMenuItem();
			byLongitudeToolStripMenuItem = new ToolStripMenuItem();
			byLatitudeToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)computeUsingToolStripMenuItem,
				(ToolStripItem)cmbSites,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1011, 27));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)withPredictionToolStripMenuItem).set_BackColor(Color.LemonChiffon);
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)thLineBreaksToolStripMenuItem,
				(ToolStripItem)limitSitePrecisionInOutputToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(123, 23));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...     ");
			thLineBreaksToolStripMenuItem.set_Checked(true);
			thLineBreaksToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)thLineBreaksToolStripMenuItem).set_Name("thLineBreaksToolStripMenuItem");
			thLineBreaksToolStripMenuItem.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)thLineBreaksToolStripMenuItem).set_Size(new Size(325, 22));
			((ToolStripItem)thLineBreaksToolStripMenuItem).set_Text("5th Line breaks");
			((ToolStripItem)thLineBreaksToolStripMenuItem).add_Click((EventHandler)thLineBreaksToolStripMenuItem_Click);
			limitSitePrecisionInOutputToolStripMenuItem.set_Checked(true);
			limitSitePrecisionInOutputToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)limitSitePrecisionInOutputToolStripMenuItem).set_Name("limitSitePrecisionInOutputToolStripMenuItem");
			limitSitePrecisionInOutputToolStripMenuItem.set_ShortcutKeys((Keys)131148);
			((ToolStripItem)limitSitePrecisionInOutputToolStripMenuItem).set_Size(new Size(325, 22));
			((ToolStripItem)limitSitePrecisionInOutputToolStripMenuItem).set_Text("Limit site coordinate precision in output");
			((ToolStripItem)limitSitePrecisionInOutputToolStripMenuItem).add_Click((EventHandler)limitSitePrecisionInOutputToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(322, 6));
			((ToolStripItem)computeUsingToolStripMenuItem).set_BackColor(Color.PaleGreen);
			((ToolStripItem)computeUsingToolStripMenuItem).set_Name("computeUsingToolStripMenuItem");
			((ToolStripItem)computeUsingToolStripMenuItem).set_Size(new Size(120, 23));
			((ToolStripItem)computeUsingToolStripMenuItem).set_Text("&Compute using =>");
			((ToolStripItem)computeUsingToolStripMenuItem).set_TextAlign(ContentAlignment.MiddleRight);
			((ToolStripItem)computeUsingToolStripMenuItem).add_Click((EventHandler)computeUsingToolStripMenuItem_Click);
			((ToolStripItem)cmbSites).set_BackColor(Color.PaleGreen);
			cmbSites.set_DropDownStyle((ComboBoxStyle)2);
			cmbSites.set_MaxDropDownItems(20);
			((ToolStripItem)cmbSites).set_Name("cmbSites");
			((ToolStripItem)cmbSites).set_Size(new Size(151, 23));
			cmbSites.add_SelectedIndexChanged((EventHandler)cmbSites_SelectedIndexChanged);
			((Control)lstPredictions).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPredictions).set_FormattingEnabled(true);
			lstPredictions.set_HorizontalExtent(1750);
			lstPredictions.set_HorizontalScrollbar(true);
			lstPredictions.set_ItemHeight(14);
			((Control)lstPredictions).set_Location(new Point(12, 36));
			((Control)lstPredictions).set_Name("lstPredictions");
			lstPredictions.set_SelectionMode((SelectionMode)3);
			((Control)lstPredictions).set_Size(new Size(987, 494));
			((Control)lstPredictions).set_TabIndex(1);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(325, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy Selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(325, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy ALL");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(325, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(325, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(325, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)sortToolStripMenuItem).set_BackColor(Color.Bisque);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)bySiteNameToolStripMenuItem,
				(ToolStripItem)byLongitudeToolStripMenuItem,
				(ToolStripItem)byLatitudeToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageAlign(ContentAlignment.MiddleRight);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(77, 23));
			((ToolStripItem)sortToolStripMenuItem).set_Text("&Sort       ");
			((ToolStripItem)sortToolStripMenuItem).set_TextAlign(ContentAlignment.MiddleRight);
			((ToolStripItem)bySiteNameToolStripMenuItem).set_Name("bySiteNameToolStripMenuItem");
			((ToolStripItem)bySiteNameToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)bySiteNameToolStripMenuItem).set_Text("by site name");
			((ToolStripItem)bySiteNameToolStripMenuItem).add_Click((EventHandler)bySiteNameToolStripMenuItem_Click);
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Name("byLongitudeToolStripMenuItem");
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Text("by Longitude");
			((ToolStripItem)byLongitudeToolStripMenuItem).add_Click((EventHandler)byLongitudeToolStripMenuItem_Click);
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Name("byLatitudeToolStripMenuItem");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Text("by Latitude");
			((ToolStripItem)byLatitudeToolStripMenuItem).add_Click((EventHandler)byLatitudeToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 23));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help     ");
			((ToolStripItem)helpToolStripMenuItem).set_TextAlign(ContentAlignment.MiddleRight);
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_BackColor(Color.MistyRose);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(69, 23));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit     ");
			((ToolStripItem)exitToolStripMenuItem).set_TextAlign(ContentAlignment.MiddleRight);
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1011, 580));
			((Control)this).get_Controls().Add((Control)(object)lstPredictions);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterContactTimes", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationAsterContactTimes);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidPlanetContactTimes");
			((Control)this).set_Text("Planet  Contact  Times");
			((Form)this).add_Load((EventHandler)AsteroidPlanetContactTimes_Load);
			((Control)this).add_Resize((EventHandler)AsteroidPlanetContactTimes_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
