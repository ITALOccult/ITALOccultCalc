using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class DisplayAsteroidsByClass : Form
	{
		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		public ListBox lstDistant;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNameToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNumberToolStripMenuItem;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem byClassToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public DisplayAsteroidsByClass()
		{
			InitializeComponent();
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
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(CollectEvents(), "Distant occultations", Settings.Default.Save_AsteroidResults);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstDistant.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstDistant.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void byAsteroidNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DistantAsteroid.SortField = 0;
			Asteroid_Observations_Reports.DistantList.Sort();
			Display();
		}

		private void byAsteroidNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DistantAsteroid.SortField = 1;
			Asteroid_Observations_Reports.DistantList.Sort();
			Display();
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DistantAsteroid.SortField = 2;
			Asteroid_Observations_Reports.DistantList.Sort();
			Display();
		}

		private void byClassToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DistantAsteroid.SortField = 3;
			Asteroid_Observations_Reports.DistantList.Sort();
			Display();
		}

		public void Display()
		{
			int num = 0;
			lstDistant.get_Items().Clear();
			lstDistant.get_Items().Add((object)(Asteroid_Observations_Reports.DistantList.Count + " events by asteroid class (other than Main-belt)"));
			lstDistant.get_Items().Add((object)"");
			for (int i = 0; i < Asteroid_Observations_Reports.DistantList.Count; i++)
			{
				if (DistantAsteroid.SortField == 3 && i > 0 && Asteroid_Observations_Reports.DistantList[i].Class != Asteroid_Observations_Reports.DistantList[i - 1].Class)
				{
					lstDistant.get_Items().Add((object)"");
					lstDistant.get_Items().Add((object)"");
					num = 0;
				}
				lstDistant.get_Items().Add((object)Asteroid_Observations_Reports.DistantList[i].ToString());
				num++;
				if (num % 5 == 0)
				{
					lstDistant.get_Items().Add((object)"");
				}
			}
		}

		private void DisplayDistantEvents_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Height() > 100)
			{
				((Control)lstDistant).set_Height(((Control)this).get_Height() - 70);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"TNO asteroid events - Display");
		}

		private void DisplayDistantEvents_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : v." + Utilities.OccultVersion_Short);
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
			//IL_0701: Unknown result type (might be due to invalid IL or missing references)
			//IL_070b: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNameToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNumberToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			byClassToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstDistant = new ListBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(452, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(81, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)byAsteroidNameToolStripMenuItem,
				(ToolStripItem)byAsteroidNumberToolStripMenuItem,
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)byClassToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(74, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...   ");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Name("byAsteroidNameToolStripMenuItem");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Size(new Size(179, 22));
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Text("by Asteroid name");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).add_Click((EventHandler)byAsteroidNameToolStripMenuItem_Click);
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Name("byAsteroidNumberToolStripMenuItem");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Size(new Size(179, 22));
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Text("by Asteroid number");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).add_Click((EventHandler)byAsteroidNumberToolStripMenuItem_Click);
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(179, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("by Date");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)byClassToolStripMenuItem).set_Name("byClassToolStripMenuItem");
			((ToolStripItem)byClassToolStripMenuItem).set_Size(new Size(179, 22));
			((ToolStripItem)byClassToolStripMenuItem).set_Text("by Class");
			((ToolStripItem)byClassToolStripMenuItem).add_Click((EventHandler)byClassToolStripMenuItem_Click);
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
			((Control)lstDistant).set_Anchor((AnchorStyles)1);
			((Control)lstDistant).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDistant).set_FormattingEnabled(true);
			lstDistant.set_ItemHeight(14);
			((Control)lstDistant).set_Location(new Point(7, 32));
			((Control)lstDistant).set_Name("lstDistant");
			((Control)lstDistant).set_Size(new Size(437, 326));
			((Control)lstDistant).set_TabIndex(1);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(452, 365));
			((Control)this).get_Controls().Add((Control)(object)lstDistant);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterShowDistant", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationAsterShowDistant);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("DisplayAsteroidsByClass");
			((Control)this).set_Text("Events by asteroid classes (other than Main-belt)");
			((Form)this).add_Load((EventHandler)DisplayDistantEvents_Load);
			((Control)this).add_Resize((EventHandler)DisplayDistantEvents_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
