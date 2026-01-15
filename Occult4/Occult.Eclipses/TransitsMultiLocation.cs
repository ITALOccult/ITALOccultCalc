using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Eclipses
{
	public class TransitsMultiLocation : Form
	{
		private bool Add5th = true;

		private IContainer components;

		private ToolStripMenuItem byNameToolStripMenuItem;

		private ToolStripMenuItem byLongitudeToolStripMenuItem;

		private ToolStripMenuItem byLatitudeToolStripMenuItem;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem1;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem toolStripMenuItem3;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem include5thlineBreaksToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstPredictions;

		private ToolStripMenuItem helpToolStripMenuItem;

		public TransitsMultiLocation()
		{
			InitializeComponent();
		}

		internal void Display()
		{
			TransitMaths.MultiEvent.Sort();
			lstPredictions.BeginUpdate();
			lstPredictions.get_Items().Clear();
			lstPredictions.get_Items().Add((object)(TransitMaths.TransitLabel + "- multisite predictions"));
			lstPredictions.get_Items().Add((object)"");
			lstPredictions.get_Items().Add((object)"Note: '+' after the time => following UT day; '-' after the time => preceding UT day; ");
			lstPredictions.get_Items().Add((object)"");
			lstPredictions.get_Items().Add((object)"                                                               Exterior INGRESS        Interior INGRESS         Maximum             Interior EGRESS      Exterior EGRESS          Min.");
			lstPredictions.get_Items().Add((object)"Site                              Longitude Latitude  Elvn      U.T.      PA  VA Alt    U.T.      PA  VA     U.T.      PA  VA Alt    U.T.      PA  VA     U.T.      PA  VA Alt    Sepn");
			lstPredictions.get_Items().Add((object)"                                    o   '    o   '       m     h  m  s     o   o  o    h  m  s     o   o    h  m  s     o   o  o    h  m  s     o   o    h  m  s     o   o  o       \"");
			for (int i = 0; i < TransitMaths.MultiEvent.Count; i++)
			{
				lstPredictions.get_Items().Add((object)TransitMaths.MultiEvent[i].ToString());
				if (((i + 1) % 5 == 0) & Add5th)
				{
					lstPredictions.get_Items().Add((object)"");
				}
			}
			lstPredictions.EndUpdate();
			((Control)this).Focus();
		}

		private void toolStripMenuItem1_Click(object sender, EventArgs e)
		{
			EclipseContactTimes.SortField = 0;
			Display();
		}

		private void toolStripMenuItem2_Click(object sender, EventArgs e)
		{
			EclipseContactTimes.SortField = 1;
			Display();
		}

		private void toolStripMenuItem3_Click(object sender, EventArgs e)
		{
			EclipseContactTimes.SortField = 2;
			Display();
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
			Settings.Default.Save_Transits = Output.SaveAppendPredictionText(CollectEvents(), TransitMaths.TransitLabel.Replace(" ", "") + "_Multisites", Settings.Default.Save_Transits);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void TransitsMultiLocation_Resize(object sender, EventArgs e)
		{
			((Control)lstPredictions).set_Width(((Control)this).get_Width() - 16);
			((Control)lstPredictions).set_Height(((Control)this).get_Height() - 65);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Transits - MultiLocation");
		}

		private void TransitsMultiLocation_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
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
			//IL_0819: Unknown result type (might be due to invalid IL or missing references)
			//IL_0823: Expected O, but got Unknown
			byNameToolStripMenuItem = new ToolStripMenuItem();
			byLongitudeToolStripMenuItem = new ToolStripMenuItem();
			byLatitudeToolStripMenuItem = new ToolStripMenuItem();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			include5thlineBreaksToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripMenuItem3 = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstPredictions = new ListBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStripItem)byNameToolStripMenuItem).set_Name("byNameToolStripMenuItem");
			((ToolStripItem)byNameToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byNameToolStripMenuItem).set_Text("by Name");
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Name("byLongitudeToolStripMenuItem");
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Text("by Longitude");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Name("byLatitudeToolStripMenuItem");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Text("by Latitude");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(882, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)include5thlineBreaksToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(120, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("    with Prediction...");
			include5thlineBreaksToolStripMenuItem.set_Checked(true);
			include5thlineBreaksToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Name("include5thlineBreaksToolStripMenuItem");
			include5thlineBreaksToolStripMenuItem.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).set_Text("Include 5th-line breaks");
			((ToolStripItem)include5thlineBreaksToolStripMenuItem).add_Click((EventHandler)include5thlineBreaksToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(231, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(234, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
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
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)toolStripMenuItem1,
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)toolStripMenuItem3
			});
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(49, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...");
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem1).set_Text("by Name");
			((ToolStripItem)toolStripMenuItem1).add_Click((EventHandler)toolStripMenuItem1_Click);
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem2).set_Text("by Longitude");
			((ToolStripItem)toolStripMenuItem2).add_Click((EventHandler)toolStripMenuItem2_Click);
			((ToolStripItem)toolStripMenuItem3).set_Name("toolStripMenuItem3");
			((ToolStripItem)toolStripMenuItem3).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem3).set_Text("by Latitude");
			((ToolStripItem)toolStripMenuItem3).add_Click((EventHandler)toolStripMenuItem3_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstPredictions).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPredictions).set_FormattingEnabled(true);
			lstPredictions.set_HorizontalExtent(1200);
			lstPredictions.set_HorizontalScrollbar(true);
			lstPredictions.set_ItemHeight(14);
			((Control)lstPredictions).set_Location(new Point(0, 26));
			((Control)lstPredictions).set_Name("lstPredictions");
			((Control)lstPredictions).set_Size(new Size(882, 480));
			((Control)lstPredictions).set_TabIndex(1);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(882, 509));
			((Control)this).get_Controls().Add((Control)(object)lstPredictions);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseTransitMulti", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationEclipseTransitMulti);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("TransitsMultiLocation");
			((Control)this).set_Text("Transits   Multi-Location predictions");
			((Form)this).add_Load((EventHandler)TransitsMultiLocation_Load);
			((Control)this).add_Resize((EventHandler)TransitsMultiLocation_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
