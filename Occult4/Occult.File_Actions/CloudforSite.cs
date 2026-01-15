using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.File_Actions
{
	public class CloudforSite : Form
	{
		private IContainer components;

		internal ListBox lstCloud;

		internal Label label1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem cloudMapToolStripMenuItem;

		public CloudforSite()
		{
			InitializeComponent();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstCloud.get_Items().get_Count() >= 1)
			{
				string text = ((Control)label1).get_Text() + "\r\n";
				for (int i = 0; i < lstCloud.get_Items().get_Count(); i++)
				{
					text = text + "\r\n" + lstCloud.get_Items().get_Item(i).ToString();
				}
				Clipboard.SetText(text);
			}
		}

		private void CloudforSite_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
		}

		private void cloudMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			APanelWeatherInfo.ShowCloudMap();
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
			//IL_0365: Unknown result type (might be due to invalid IL or missing references)
			//IL_036f: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(CloudforSite));
			lstCloud = new ListBox();
			label1 = new Label();
			menuStrip1 = new MenuStrip();
			copyToolStripMenuItem = new ToolStripMenuItem();
			cloudMapToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstCloud).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstCloud).set_FormattingEnabled(true);
			lstCloud.set_ItemHeight(14);
			((Control)lstCloud).set_Location(new Point(10, 45));
			((Control)lstCloud).set_Name("lstCloud");
			((Control)lstCloud).set_Size(new Size(218, 368));
			((Control)lstCloud).set_TabIndex(0);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(12, 29));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(35, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("label1");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)cloudMapToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(236, 24));
			((Control)menuStrip1).set_TabIndex(2);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(63, 20));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)cloudMapToolStripMenuItem).set_Image((Image)Resources.EARTH);
			((ToolStripItem)cloudMapToolStripMenuItem).set_Name("cloudMapToolStripMenuItem");
			((ToolStripItem)cloudMapToolStripMenuItem).set_Size(new Size(94, 20));
			((ToolStripItem)cloudMapToolStripMenuItem).set_Text("Cloud map");
			((ToolStripItem)cloudMapToolStripMenuItem).add_Click((EventHandler)cloudMapToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(236, 419));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstCloud);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationFileCloudForSite", true, (DataSourceUpdateMode)1));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationFileCloudForSite);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("CloudforSite");
			((Control)this).set_Text("Cloud and temperature");
			((Form)this).add_Load((EventHandler)CloudforSite_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
