using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class ExportComments : Form
	{
		private IContainer components;

		internal TextBox txtComments;

		public Label label1;

		public ExportComments(bool WhenExporting)
		{
			InitializeComponent();
			((Control)txtComments).set_Text("");
			if (!WhenExporting)
			{
				((Control)label1).set_Text("Review these comments about observer lines from the Export file");
			}
		}

		private void ExportComments_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() < 100)
			{
				((Control)this).set_Width(100);
			}
			if (((Control)this).get_Height() < 200)
			{
				((Control)this).set_Height(200);
			}
			((Control)txtComments).set_Width(((Control)this).get_Width() - 31);
			((Control)txtComments).set_Height(((Control)this).get_Height() - 98);
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
			txtComments = new TextBox();
			label1 = new Label();
			((Control)this).SuspendLayout();
			txtComments.set_AcceptsReturn(true);
			((TextBoxBase)txtComments).set_AcceptsTab(true);
			((Control)txtComments).set_AllowDrop(true);
			((Control)txtComments).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtComments).set_Location(new Point(8, 47));
			((TextBoxBase)txtComments).set_Multiline(true);
			((Control)txtComments).set_Name("txtComments");
			txtComments.set_ScrollBars((ScrollBars)2);
			((Control)txtComments).set_Size(new Size(446, 393));
			((Control)txtComments).set_TabIndex(0);
			((TextBoxBase)txtComments).set_WordWrap(false);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(14, 15));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(78, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Comments in");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(461, 452));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtComments);
			((Control)this).set_Name("ExportComments");
			((Control)this).set_Text("Comments in the Export file");
			((Control)this).add_Resize((EventHandler)ExportComments_Resize);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
