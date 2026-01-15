using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class DoubleList : Form
	{
		private IContainer components;

		private Label label1;

		internal TextBox txtDoubles;

		public DoubleList()
		{
			InitializeComponent();
		}

		private void DoubleList_Load(object sender, EventArgs e)
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
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			txtDoubles = new TextBox();
			label1 = new Label();
			((Control)this).SuspendLayout();
			((Control)txtDoubles).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDoubles).set_Location(new Point(8, 39));
			((TextBoxBase)txtDoubles).set_Multiline(true);
			((Control)txtDoubles).set_Name("txtDoubles");
			txtDoubles.set_ScrollBars((ScrollBars)2);
			((Control)txtDoubles).set_Size(new Size(273, 141));
			((Control)txtDoubles).set_TabIndex(0);
			((Control)label1).set_Location(new Point(3, 8));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(282, 28));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("The following events have a code indicating it is a double star, or that no duration is given for a gradual event");
			label1.set_TextAlign(ContentAlignment.MiddleCenter);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(289, 188));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtDoubles);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarDoubleList", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarDoubleList);
			((Control)this).set_Name("DoubleList");
			((Control)this).set_Text("Double star list");
			((Form)this).add_Load((EventHandler)DoubleList_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
