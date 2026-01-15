using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.BailyBeads
{
	public class Results : Form
	{
		private IContainer components;

		internal CheckedListBox boxEvents;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byTimeToolStripMenuItem;

		private ToolStripMenuItem byAxisAngleToolStripMenuItem;

		private ToolStripMenuItem solveToolStripMenuItem;

		private ToolStripMenuItem deleteSelectedToolStripMenuItem;

		private ToolStripMenuItem selectedToolStripMenuItem;

		private ToolStripMenuItem allObservationsToolStripMenuItem;

		private Label label1;

		internal Label lblSolution;

		private ToolStripMenuItem OnTop;

		private ToolStripMenuItem helpToolStripMenuItem;

		public Results()
		{
			InitializeComponent();
		}

		internal void UpdateBox()
		{
			((ListBox)boxEvents).BeginUpdate();
			((ObjectCollection)boxEvents.get_Items()).Clear();
			for (int i = 0; i < BailyBeads.Observations.Count; i++)
			{
				int num = ((ObjectCollection)boxEvents.get_Items()).Add((object)BailyBeads.Observations[i].TextLine);
				boxEvents.SetItemChecked(num, BailyBeads.Observations[i].Check);
			}
			((ListBox)boxEvents).EndUpdate();
		}

		private void byTimeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BailyResults.SortField = 0;
			BailyBeads.Observations.Sort();
			UpdateBox();
		}

		private void byAxisAngleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BailyResults.SortField = 1;
			BailyBeads.Observations.Sort();
			UpdateBox();
		}

		private void Results_Resize(object sender, EventArgs e)
		{
			((Control)boxEvents).set_Height(((Control)this).get_Height() - 95);
		}

		private void selectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_0046: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)boxEvents).get_SelectedIndex();
			string text = ((ObjectCollection)boxEvents.get_Items()).get_Item(selectedIndex).ToString();
			if ((int)MessageBox.Show("Do you want to delete\r\n\r\n" + text, "Confirm Delete", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			BailyBeads.Observations.RemoveAt(selectedIndex);
			UpdateBox();
			if (selectedIndex < ((ObjectCollection)boxEvents.get_Items()).get_Count())
			{
				((ListControl)boxEvents).set_SelectedIndex(selectedIndex);
				return;
			}
			selectedIndex--;
			if (selectedIndex >= 0)
			{
				((ListControl)boxEvents).set_SelectedIndex(selectedIndex);
			}
		}

		private void allObservationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to delete ALL observations?", "Confirm Delete", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				BailyBeads.Observations.Clear();
				UpdateBox();
			}
		}

		private void boxEvents_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			int selectedIndex = ((ListControl)boxEvents).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				BailyBeads.TransferObservationToPlotTime(selectedIndex);
			}
		}

		private void solveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BailyBeads.Solve();
		}

		private void Results_Load(object sender, EventArgs e)
		{
			((Control)this).set_Height(((Control)BailyBeads.BailyBeads_Main).get_Height());
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : v." + Utilities.OccultVersion_Short);
		}

		private void OnTop_Click(object sender, EventArgs e)
		{
			((Form)this).set_TopMost(!((Form)this).get_TopMost());
			if (((Form)this).get_TopMost())
			{
				((ToolStripItem)OnTop).set_Text("&Normal");
			}
			else
			{
				((ToolStripItem)OnTop).set_Text("on&Top");
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Baily bead - results");
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
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_05fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0607: Expected O, but got Unknown
			boxEvents = new CheckedListBox();
			menuStrip1 = new MenuStrip();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byTimeToolStripMenuItem = new ToolStripMenuItem();
			byAxisAngleToolStripMenuItem = new ToolStripMenuItem();
			deleteSelectedToolStripMenuItem = new ToolStripMenuItem();
			selectedToolStripMenuItem = new ToolStripMenuItem();
			allObservationsToolStripMenuItem = new ToolStripMenuItem();
			solveToolStripMenuItem = new ToolStripMenuItem();
			OnTop = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			lblSolution = new Label();
			label1 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)boxEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)boxEvents).set_FormattingEnabled(true);
			((Control)boxEvents).set_Location(new Point(6, 52));
			((Control)boxEvents).set_Name("boxEvents");
			((Control)boxEvents).set_Size(new Size(261, 319));
			((Control)boxEvents).set_TabIndex(0);
			((Control)boxEvents).add_MouseDoubleClick(new MouseEventHandler(boxEvents_MouseDoubleClick));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)deleteSelectedToolStripMenuItem,
				(ToolStripItem)solveToolStripMenuItem,
				(ToolStripItem)OnTop,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(274, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)byTimeToolStripMenuItem,
				(ToolStripItem)byAxisAngleToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(52, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort... ");
			((ToolStripItem)byTimeToolStripMenuItem).set_Name("byTimeToolStripMenuItem");
			((ToolStripItem)byTimeToolStripMenuItem).set_Size(new Size(145, 22));
			((ToolStripItem)byTimeToolStripMenuItem).set_Text("by Time");
			((ToolStripItem)byTimeToolStripMenuItem).add_Click((EventHandler)byTimeToolStripMenuItem_Click);
			((ToolStripItem)byAxisAngleToolStripMenuItem).set_Name("byAxisAngleToolStripMenuItem");
			((ToolStripItem)byAxisAngleToolStripMenuItem).set_Size(new Size(145, 22));
			((ToolStripItem)byAxisAngleToolStripMenuItem).set_Text("by Axis Angle");
			((ToolStripItem)byAxisAngleToolStripMenuItem).add_Click((EventHandler)byAxisAngleToolStripMenuItem_Click);
			((ToolStripDropDownItem)deleteSelectedToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)selectedToolStripMenuItem,
				(ToolStripItem)allObservationsToolStripMenuItem
			});
			((ToolStripItem)deleteSelectedToolStripMenuItem).set_Name("deleteSelectedToolStripMenuItem");
			((ToolStripItem)deleteSelectedToolStripMenuItem).set_Size(new Size(64, 20));
			((ToolStripItem)deleteSelectedToolStripMenuItem).set_Text("Delete... ");
			((ToolStripItem)selectedToolStripMenuItem).set_Name("selectedToolStripMenuItem");
			((ToolStripItem)selectedToolStripMenuItem).set_Size(new Size(158, 22));
			((ToolStripItem)selectedToolStripMenuItem).set_Text("Selected");
			((ToolStripItem)selectedToolStripMenuItem).add_Click((EventHandler)selectedToolStripMenuItem_Click);
			((ToolStripItem)allObservationsToolStripMenuItem).set_Name("allObservationsToolStripMenuItem");
			((ToolStripItem)allObservationsToolStripMenuItem).set_Size(new Size(158, 22));
			((ToolStripItem)allObservationsToolStripMenuItem).set_Text("All observations");
			((ToolStripItem)allObservationsToolStripMenuItem).add_Click((EventHandler)allObservationsToolStripMenuItem_Click);
			((ToolStripItem)solveToolStripMenuItem).set_Name("solveToolStripMenuItem");
			((ToolStripItem)solveToolStripMenuItem).set_Size(new Size(47, 20));
			((ToolStripItem)solveToolStripMenuItem).set_Text("&Solve");
			((ToolStripItem)solveToolStripMenuItem).add_Click((EventHandler)solveToolStripMenuItem_Click);
			((ToolStripItem)OnTop).set_Name("OnTop");
			((ToolStripItem)OnTop).set_Size(new Size(54, 20));
			((ToolStripItem)OnTop).set_Text("on&Top");
			((ToolStripItem)OnTop).add_Click((EventHandler)OnTop_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(44, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)lblSolution).set_AutoSize(true);
			((Control)lblSolution).set_Location(new Point(3, 24));
			((Control)lblSolution).set_Name("lblSolution");
			((Control)lblSolution).set_Size(new Size(32, 13));
			((Control)lblSolution).set_TabIndex(2);
			((Control)lblSolution).set_Text("[Sun]");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(54, 38));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(138, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("double-click to plot an event");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(274, 376));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lblSolution);
			((Control)this).get_Controls().Add((Control)(object)boxEvents);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationBailyResults", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationBailyResults);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MaximumSize(new Size(290, 2000));
			((Control)this).set_MinimumSize(new Size(290, 36));
			((Control)this).set_Name("Results");
			((Control)this).set_Text("Observations");
			((Form)this).add_Load((EventHandler)Results_Load);
			((Control)this).add_Resize((EventHandler)Results_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
