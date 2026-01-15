using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class SelectSatellite : Form
	{
		private IContainer components;

		private Label label1;

		internal ListBox lstSatellites;

		private Label label2;

		private Label label3;

		public SelectSatellite()
		{
			InitializeComponent();
		}

		private void lstSatellites_DoubleClick(object sender, EventArgs e)
		{
			((Form)this).Close();
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
			lstSatellites = new ListBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			((Control)this).SuspendLayout();
			((Control)lstSatellites).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSatellites).set_FormattingEnabled(true);
			lstSatellites.set_ItemHeight(14);
			((Control)lstSatellites).set_Location(new Point(7, 57));
			((Control)lstSatellites).set_Name("lstSatellites");
			((Control)lstSatellites).set_Size(new Size(242, 130));
			((Control)lstSatellites).set_TabIndex(0);
			((Control)lstSatellites).add_DoubleClick((EventHandler)lstSatellites_DoubleClick);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(2, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(253, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Double-click on the satellite solution to use");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(4, 28));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(101, 13));
			((Control)label2).set_TabIndex(2);
			((Control)label2).set_Text("Compnt          Name");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(27, 42));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(176, 13));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("Soln                      (Solution method)");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(256, 190));
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstSatellites);
			((Form)this).set_FormBorderStyle((FormBorderStyle)5);
			((Control)this).set_Name("SelectSatellite");
			((Control)this).set_Text("Select Satellite");
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
