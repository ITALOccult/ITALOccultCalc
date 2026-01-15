using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class ListDuplicates : Form
	{
		private IContainer components;

		internal ListBox lstDuplicates;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private Button cmdContinue;

		private Button cmdCancel;

		public ListDuplicates()
		{
			InitializeComponent();
		}

		internal void SetForArchiving(bool ForArchiving)
		{
			if (ForArchiving)
			{
				((ToolStripItem)exitToolStripMenuItem).set_Visible(false);
				Button obj = cmdCancel;
				bool visible;
				((Control)cmdContinue).set_Visible(visible = true);
				((Control)obj).set_Visible(visible);
			}
			else
			{
				((ToolStripItem)exitToolStripMenuItem).set_Visible(true);
				Button obj2 = cmdCancel;
				bool visible;
				((Control)cmdContinue).set_Visible(visible = false);
				((Control)obj2).set_Visible(visible);
			}
		}

		private void ListDuplicates_Resize(object sender, EventArgs e)
		{
			((Control)lstDuplicates).set_Width(((Control)this).get_Width() - 37);
			((Control)lstDuplicates).set_Height(((Control)this).get_Height() - 67);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SavePredictionText(CollectEvents(), "Archive Duplicates", Utilities.AppPath + "\\Observations");
		}

		private string CollectEvents()
		{
			string text = "";
			for (int i = 0; i < lstDuplicates.get_Items().get_Count(); i++)
			{
				text = text + lstDuplicates.get_Items().get_Item(i).ToString() + "\r\n";
			}
			return text;
		}

		private void cmdContinue_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)1);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)2);
		}

		private void ListDuplicates_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
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
			lstDuplicates = new ListBox();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdContinue = new Button();
			cmdCancel = new Button();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstDuplicates).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDuplicates).set_FormattingEnabled(true);
			lstDuplicates.set_ItemHeight(14);
			((Control)lstDuplicates).set_Location(new Point(8, 29));
			((Control)lstDuplicates).set_Name("lstDuplicates");
			((Control)lstDuplicates).set_Size(new Size(565, 284));
			((Control)lstDuplicates).set_TabIndex(0);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(585, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(87, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List....    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdContinue).set_Location(new Point(172, 4));
			((Control)cmdContinue).set_Name("cmdContinue");
			((Control)cmdContinue).set_Size(new Size(160, 21));
			((Control)cmdContinue).set_TabIndex(2);
			((Control)cmdContinue).set_Text("Continue - Archive file creation");
			((ButtonBase)cmdContinue).set_UseVisualStyleBackColor(true);
			((Control)cmdContinue).add_Click((EventHandler)cmdContinue_Click);
			((Control)cmdCancel).set_Location(new Point(387, 4));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(160, 21));
			((Control)cmdCancel).set_TabIndex(3);
			((Control)cmdCancel).set_Text("Cancel - Archive file creation");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(585, 316));
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdContinue);
			((Control)this).get_Controls().Add((Control)(object)lstDuplicates);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("ListDuplicates");
			((Control)this).set_Text("List Duplicate entries in an Archive file");
			((Form)this).add_Load((EventHandler)ListDuplicates_Load);
			((Control)this).add_Resize((EventHandler)ListDuplicates_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
