using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class Event_coordinates : Form
	{
		private IContainer components;

		internal ListBox lstCoords;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private CheckBox chkIncludeUncertainties;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem saveSelectedToolStripMenuItem;

		public Event_coordinates()
		{
			InitializeComponent();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SavePredictionText(CollectEvents(Selected: false), "Event coordinates (" + EventDetails.AsteroidNumber + ") " + EventDetails.FormattedDate, Utilities.AppPath + "\\Asteroid");
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents(Selected: false));
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(Selected: false));
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private string CollectEvents(bool Selected)
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (Selected)
			{
				for (int i = 0; i < 3; i++)
				{
					stringBuilder.AppendLine(lstCoords.get_Items().get_Item(i).ToString());
				}
			}
			if (Selected)
			{
				foreach (object selectedItem in lstCoords.get_SelectedItems())
				{
					stringBuilder.AppendLine(selectedItem.ToString());
				}
			}
			else
			{
				foreach (object item in lstCoords.get_Items())
				{
					stringBuilder.AppendLine(item.ToString());
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void Event_coordinates_Activated(object sender, EventArgs e)
		{
			Data_and_Plots.EventCoordinates(chkIncludeUncertainties.get_Checked());
			Application.DoEvents();
		}

		private void chkIncludeUncertainties_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.EventCoordinates(chkIncludeUncertainties.get_Checked());
		}

		private void Event_coordinates_Resize(object sender, EventArgs e)
		{
			((Control)lstCoords).set_Width(((Control)this).get_Width() - 26);
			((Control)lstCoords).set_Height(((Control)this).get_Height() - 75);
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(Selected: true));
		}

		private void saveSelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SavePredictionText(CollectEvents(Selected: true), "Event coordinates (" + EventDetails.AsteroidNumber + ") " + EventDetails.FormattedDate, Utilities.AppPath + "\\Asteroid");
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
			lstCoords = new ListBox();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveSelectedToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			chkIncludeUncertainties = new CheckBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstCoords).set_Font(new Font("Cascadia Code", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstCoords).set_FormattingEnabled(true);
			lstCoords.set_ItemHeight(15);
			((Control)lstCoords).set_Location(new Point(5, 27));
			((Control)lstCoords).set_Name("lstCoords");
			lstCoords.set_SelectionMode((SelectionMode)3);
			((Control)lstCoords).set_Size(new Size(791, 289));
			((Control)lstCoords).set_TabIndex(3);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(800, 24));
			((Control)menuStrip1).set_TabIndex(4);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveSelectedToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(411, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...                                                                                                                 ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy All");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save All");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveSelectedToolStripMenuItem).set_Name("saveSelectedToolStripMenuItem");
			((ToolStripItem)saveSelectedToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)saveSelectedToolStripMenuItem).set_Text("Save selected");
			((ToolStripItem)saveSelectedToolStripMenuItem).add_Click((EventHandler)saveSelectedToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)chkIncludeUncertainties).set_AutoSize(true);
			((Control)chkIncludeUncertainties).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkIncludeUncertainties).set_ForeColor(Color.DarkRed);
			((Control)chkIncludeUncertainties).set_Location(new Point(94, 5));
			((Control)chkIncludeUncertainties).set_Name("chkIncludeUncertainties");
			((Control)chkIncludeUncertainties).set_Size(new Size(213, 17));
			((Control)chkIncludeUncertainties).set_TabIndex(5);
			((Control)chkIncludeUncertainties).set_Text("Include event uncertainty coords");
			((ButtonBase)chkIncludeUncertainties).set_UseVisualStyleBackColor(true);
			chkIncludeUncertainties.add_CheckedChanged((EventHandler)chkIncludeUncertainties_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(800, 325));
			((Control)this).get_Controls().Add((Control)(object)chkIncludeUncertainties);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lstCoords);
			((Control)this).set_Name("Event_coordinates");
			((Control)this).set_Text("Event_coordinates");
			((Form)this).add_Activated((EventHandler)Event_coordinates_Activated);
			((Control)this).add_Resize((EventHandler)Event_coordinates_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
