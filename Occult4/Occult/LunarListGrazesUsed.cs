using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class LunarListGrazesUsed : Form
	{
		internal bool Predictions;

		private IContainer components;

		private ListBox lstGrazes;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem sortByDateToolStripMenuItem;

		private ToolStripMenuItem sortByStarIDToolStripMenuItem;

		private ToolStripMenuItem sortByLLibrationToolStripMenuItem;

		private ToolStripMenuItem sortByBLibrationToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem sortByOfEventsToolStripMenuItem;

		private Label label1;

		private ToolStripMenuItem helpToolStripMenuItem;

		public LunarListGrazesUsed()
		{
			InitializeComponent();
		}

		private void LunarListGrazesUsed_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			sortByOfEventsToolStripMenuItem_Click(sender, e);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		internal void Display()
		{
			int num = 0;
			lstGrazes.get_Items().Clear();
			lstGrazes.get_Items().Add((object)" Yr Mth Dy   Star         l     b    #");
			if (Predictions)
			{
				for (int i = 0; i < LunarProfile.GrazeList.Count; i++)
				{
					lstGrazes.get_Items().Add((object)LunarProfile.GrazeList[i].ToString());
					num += LunarProfile.GrazeList[i].Count;
					if (i % 5 == 4)
					{
						lstGrazes.get_Items().Add((object)"");
					}
				}
				lstGrazes.get_Items().Add((object)"");
				lstGrazes.get_Items().Add((object)("Total observations = " + LunarProfile.AllCount));
				lstGrazes.get_Items().Add((object)("Total graze observations = " + num));
				return;
			}
			for (int j = 0; j < ReductionProfile.GrazeList.Count; j++)
			{
				lstGrazes.get_Items().Add((object)ReductionProfile.GrazeList[j].ToString());
				num += ReductionProfile.GrazeList[j].Count;
				if (j % 5 == 4)
				{
					lstGrazes.get_Items().Add((object)"");
				}
			}
			lstGrazes.get_Items().Add((object)"");
			lstGrazes.get_Items().Add((object)("Total observations = " + ReductionProfile.AllCount));
			lstGrazes.get_Items().Add((object)("Total graze observations = " + num));
		}

		private void sortByDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarGrazeID.SortField = 0;
			if (Predictions)
			{
				LunarProfile.GrazeList.Sort();
			}
			else
			{
				ReductionProfile.GrazeList.Sort();
			}
			Display();
		}

		private void sortByStarIDToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarGrazeID.SortField = 1;
			if (Predictions)
			{
				LunarProfile.GrazeList.Sort();
			}
			else
			{
				ReductionProfile.GrazeList.Sort();
			}
			Display();
		}

		private void sortByLLibrationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarGrazeID.SortField = 2;
			if (Predictions)
			{
				LunarProfile.GrazeList.Sort();
			}
			else
			{
				ReductionProfile.GrazeList.Sort();
			}
			Display();
		}

		private void sortByBLibrationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarGrazeID.SortField = 3;
			if (Predictions)
			{
				LunarProfile.GrazeList.Sort();
			}
			else
			{
				ReductionProfile.GrazeList.Sort();
			}
			Display();
		}

		private void sortByOfEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarGrazeID.SortField = 4;
			if (Predictions)
			{
				LunarProfile.GrazeList.Sort();
			}
			else
			{
				ReductionProfile.GrazeList.Sort();
			}
			Display();
		}

		private string CollectEvents()
		{
			if (lstGrazes.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i < lstGrazes.get_Items().get_Count(); i++)
			{
				stringBuilder.AppendLine(lstGrazes.get_Items().get_Item(i).ToString());
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
			Settings.Default.Save_LunarResults = Output.SaveAppendPredictionText(CollectEvents(), "List of grazes used", Settings.Default.Save_LunarResults);
		}

		private void LunarListGrazesUsed_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Height() < 100) | (((Control)this).get_Width() < 100)))
			{
				((Control)lstGrazes).set_Height(((Control)this).get_Height() - 80);
			}
		}

		private void lstGrazes_DoubleClick(object sender, EventArgs e)
		{
			if (((ListControl)lstGrazes).get_SelectedIndex() < 0)
			{
				return;
			}
			string text = lstGrazes.get_Items().get_Item(((ListControl)lstGrazes).get_SelectedIndex()).ToString();
			if (text.Length >= 10)
			{
				if (Predictions)
				{
					LunarProfile.PlotGrazeProfile(HighlightRePlot: true, "", "", text.Substring(0, 11), ReReadHires: false);
				}
				else
				{
					ReductionProfile.PlotReductionProfile(HighlightRePlot: true, "", "", text.Substring(0, 11), ExcludeLargeResiduals: false, IncludeStarPath: false, IncludeAllPaths: false, ReReadHiresData: true);
				}
			}
		}

		private void LunarListGrazesUsed_Activated(object sender, EventArgs e)
		{
			Display();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Predictions - List grazes used");
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
			//IL_0798: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a2: Expected O, but got Unknown
			lstGrazes = new ListBox();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			sortByDateToolStripMenuItem = new ToolStripMenuItem();
			sortByStarIDToolStripMenuItem = new ToolStripMenuItem();
			sortByLLibrationToolStripMenuItem = new ToolStripMenuItem();
			sortByBLibrationToolStripMenuItem = new ToolStripMenuItem();
			sortByOfEventsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label1 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstGrazes).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstGrazes).set_FormattingEnabled(true);
			lstGrazes.set_ItemHeight(14);
			((Control)lstGrazes).set_Location(new Point(2, 42));
			((Control)lstGrazes).set_Name("lstGrazes");
			((Control)lstGrazes).set_Size(new Size(307, 326));
			((Control)lstGrazes).set_TabIndex(0);
			((Control)lstGrazes).add_DoubleClick((EventHandler)lstGrazes_DoubleClick);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(304, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)sortByDateToolStripMenuItem,
				(ToolStripItem)sortByStarIDToolStripMenuItem,
				(ToolStripItem)sortByLLibrationToolStripMenuItem,
				(ToolStripItem)sortByBLibrationToolStripMenuItem,
				(ToolStripItem)sortByOfEventsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(87, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List        ");
			((ToolStripItem)sortByDateToolStripMenuItem).set_Name("sortByDateToolStripMenuItem");
			sortByDateToolStripMenuItem.set_ShortcutKeys((Keys)131140);
			((ToolStripItem)sortByDateToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)sortByDateToolStripMenuItem).set_Text("sort by Date");
			((ToolStripItem)sortByDateToolStripMenuItem).add_Click((EventHandler)sortByDateToolStripMenuItem_Click);
			((ToolStripItem)sortByStarIDToolStripMenuItem).set_Name("sortByStarIDToolStripMenuItem");
			sortByStarIDToolStripMenuItem.set_ShortcutKeys((Keys)131145);
			((ToolStripItem)sortByStarIDToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)sortByStarIDToolStripMenuItem).set_Text("sort by Star ID");
			((ToolStripItem)sortByStarIDToolStripMenuItem).add_Click((EventHandler)sortByStarIDToolStripMenuItem_Click);
			((ToolStripItem)sortByLLibrationToolStripMenuItem).set_Name("sortByLLibrationToolStripMenuItem");
			sortByLLibrationToolStripMenuItem.set_ShortcutKeys((Keys)131148);
			((ToolStripItem)sortByLLibrationToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)sortByLLibrationToolStripMenuItem).set_Text("sort by L libration");
			((ToolStripItem)sortByLLibrationToolStripMenuItem).add_Click((EventHandler)sortByLLibrationToolStripMenuItem_Click);
			((ToolStripItem)sortByBLibrationToolStripMenuItem).set_Name("sortByBLibrationToolStripMenuItem");
			sortByBLibrationToolStripMenuItem.set_ShortcutKeys((Keys)131138);
			((ToolStripItem)sortByBLibrationToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)sortByBLibrationToolStripMenuItem).set_Text("sort by  B  libration");
			((ToolStripItem)sortByBLibrationToolStripMenuItem).add_Click((EventHandler)sortByBLibrationToolStripMenuItem_Click);
			((ToolStripItem)sortByOfEventsToolStripMenuItem).set_Name("sortByOfEventsToolStripMenuItem");
			sortByOfEventsToolStripMenuItem.set_ShortcutKeys((Keys)131150);
			((ToolStripItem)sortByOfEventsToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)sortByOfEventsToolStripMenuItem).set_Text("sort by # of events");
			((ToolStripItem)sortByOfEventsToolStripMenuItem).add_Click((EventHandler)sortByOfEventsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(211, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("&Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
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
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(31, 28));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(243, 12));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("double-click to highlight graze in the profile  (based on date)");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(304, 372));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstGrazes);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarGrazesUsed", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarGrazesUsed);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_MaximumSize(new Size(320, 1000));
			((Control)this).set_MinimumSize(new Size(320, 36));
			((Control)this).set_Name("LunarListGrazesUsed");
			((Control)this).set_Text("List of grazes used");
			((Form)this).add_Load((EventHandler)LunarListGrazesUsed_Load);
			((Form)this).add_Activated((EventHandler)LunarListGrazesUsed_Activated);
			((Control)this).add_Resize((EventHandler)LunarListGrazesUsed_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
