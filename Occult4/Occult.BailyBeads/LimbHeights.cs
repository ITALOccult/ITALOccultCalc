using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.BailyBeads
{
	public class LimbHeights : Form
	{
		private IContainer components;

		internal ListBox lstHeights;

		public LimbHeights()
		{
			InitializeComponent();
		}

		private void LimbHeights_Load(object sender, EventArgs e)
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
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			lstHeights = new ListBox();
			((Control)this).SuspendLayout();
			((Control)lstHeights).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstHeights).set_FormattingEnabled(true);
			lstHeights.set_ItemHeight(14);
			((Control)lstHeights).set_Location(new Point(5, 4));
			((Control)lstHeights).set_Name("lstHeights");
			((Control)lstHeights).set_Size(new Size(225, 340));
			((Control)lstHeights).set_TabIndex(0);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(235, 350));
			((Control)this).get_Controls().Add((Control)(object)lstHeights);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationBailyLimbHeights", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationBailyLimbHeights);
			((Control)this).set_Name("LimbHeights");
			((Control)this).set_Text("Limb Heights");
			((Form)this).add_Load((EventHandler)LimbHeights_Load);
			((Control)this).ResumeLayout(false);
		}
	}
}
