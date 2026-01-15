using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class MissingGrazes : Form
	{
		private IContainer components;

		private ListBox lstMissing;

		private Label label1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		public MissingGrazes()
		{
			InitializeComponent();
		}

		private void MissingGrazes_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			lstMissing.get_Items().Clear();
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Archive Obs_MissingGrazes.dat");
			do
			{
				lstMissing.get_Items().Add((object)streamReader.ReadLine());
			}
			while (!streamReader.EndOfStream);
		}

		private void MissingGrazes_Resize(object sender, EventArgs e)
		{
			((Control)lstMissing).set_Height(((Control)this).get_Height() - 155);
			((Control)lstMissing).set_Width(((Control)this).get_Width() - 29);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarObservations = Output.SavePredictionText(CollectEvents(), "Missing grazes", Settings.Default.Save_LunarObservations);
		}

		internal string CollectEvents()
		{
			string text = "Known Graze observations with no report submitted\r\n";
			for (int i = 0; i < lstMissing.get_Items().get_Count(); i++)
			{
				text = text + lstMissing.get_Items().get_Item(i).ToString() + "\r\n";
			}
			return text;
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Missing grazes");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "Known Graze observations with no report submitted\r\n";
			foreach (object selectedItem in lstMissing.get_SelectedItems())
			{
				text = text + selectedItem.ToString() + "\r\n";
			}
			Clipboard.SetText(text);
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(MissingGrazes));
			lstMissing = new ListBox();
			label1 = new Label();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstMissing).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstMissing).set_FormattingEnabled(true);
			lstMissing.set_ItemHeight(14);
			((Control)lstMissing).set_Location(new Point(6, 115));
			((Control)lstMissing).set_Name("lstMissing");
			lstMissing.set_SelectionMode((SelectionMode)3);
			((Control)lstMissing).set_Size(new Size(530, 284));
			((Control)lstMissing).set_TabIndex(1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(25, 33));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(483, 78));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text(componentResourceManager.GetString("label1.Text"));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(542, 24));
			((Control)menuStrip1).set_TabIndex(3);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...");
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
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(542, 404));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstMissing);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).set_MaximumSize(new Size(3000, 30000));
			((Control)this).set_MinimumSize(new Size(558, 300));
			((Control)this).set_Name("MissingGrazes");
			((Control)this).set_Text("Missing Grazes");
			((Form)this).add_Load((EventHandler)MissingGrazes_Load);
			((Control)this).add_Resize((EventHandler)MissingGrazes_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
