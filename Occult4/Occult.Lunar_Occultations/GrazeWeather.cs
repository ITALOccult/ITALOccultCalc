using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Occultations
{
	public class GrazeWeather : Form
	{
		internal bool Cancel;

		private IContainer components;

		internal ListBox lstWeather;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		internal ToolStripMenuItem cancelToolStripMenuItem;

		public GrazeWeather()
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
			if (lstWeather.get_Items().get_Count() >= 1)
			{
				string text = lstWeather.get_Items().get_Item(0).ToString();
				for (int i = 1; i < lstWeather.get_Items().get_Count(); i++)
				{
					text = text + "\r\n" + lstWeather.get_Items().get_Item(i).ToString();
				}
				Clipboard.SetText(text);
			}
		}

		private void cancelToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Cancel = true;
		}

		private void GrazeWeather_Load(object sender, EventArgs e)
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
			//IL_02e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f3: Expected O, but got Unknown
			lstWeather = new ListBox();
			menuStrip1 = new MenuStrip();
			copyToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cancelToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstWeather).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstWeather).set_FormattingEnabled(true);
			lstWeather.set_ItemHeight(14);
			((Control)lstWeather).set_Location(new Point(9, 34));
			((Control)lstWeather).set_Name("lstWeather");
			((Control)lstWeather).set_Size(new Size(225, 312));
			((Control)lstWeather).set_TabIndex(0);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)cancelToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(241, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy   ");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(83, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit          ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripItem)cancelToolStripMenuItem).set_Image((Image)Resources.Critical);
			((ToolStripItem)cancelToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)cancelToolStripMenuItem).set_Name("cancelToolStripMenuItem");
			((ToolStripItem)cancelToolStripMenuItem).set_Size(new Size(80, 20));
			((ToolStripItem)cancelToolStripMenuItem).set_Text("Cancel   ");
			((ToolStripItem)cancelToolStripMenuItem).add_Click((EventHandler)cancelToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(241, 354));
			((Control)this).get_Controls().Add((Control)(object)lstWeather);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarGrazeWeather", true, (DataSourceUpdateMode)1));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationLunarGrazeWeather);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("GrazeWeather");
			((Control)this).set_Text("Graze: cloud + temp.");
			((Form)this).add_Load((EventHandler)GrazeWeather_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
