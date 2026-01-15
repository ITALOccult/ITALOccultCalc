using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace AOTA
{
	public class Unistellar_SetTimes : Form
	{
		private IContainer components;

		private Label label1;

		private Label label2;

		internal Label lblStart;

		internal Label lblEnd;

		private Label label3;

		internal TextBox txtHevent;

		internal TextBox txtMevent;

		internal TextBox txtSevent;

		private Label label5;

		private Label label6;

		private Label label7;

		private Button cmdOK;

		private Label label4;

		internal TextBox txtSemiDuration;

		private Label label8;

		public Unistellar_SetTimes()
		{
			InitializeComponent();
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void txtHevent_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtHevent).SelectAll();
		}

		private void txtMevent_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMevent).SelectAll();
		}

		private void txtSevent_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSevent).SelectAll();
		}

		private void txtSemiDuration_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSemiDuration).SelectAll();
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
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			label1 = new Label();
			label2 = new Label();
			lblStart = new Label();
			lblEnd = new Label();
			label3 = new Label();
			txtHevent = new TextBox();
			txtMevent = new TextBox();
			txtSevent = new TextBox();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			cmdOK = new Button();
			label4 = new Label();
			txtSemiDuration = new TextBox();
			label8 = new Label();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(14, 26));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(118, 17));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text(".csv file starts at: ");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(14, 53));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(114, 17));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text(".csv file ends at: ");
			((Control)lblStart).set_AutoSize(true);
			((Control)lblStart).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblStart).set_Location(new Point(156, 26));
			((Control)lblStart).set_Name("lblStart");
			((Control)lblStart).set_Size(new Size(56, 17));
			((Control)lblStart).set_TabIndex(2);
			((Control)lblStart).set_Text("12:9:00");
			((Control)lblEnd).set_AutoSize(true);
			((Control)lblEnd).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEnd).set_Location(new Point(156, 53));
			((Control)lblEnd).set_Name("lblEnd");
			((Control)lblEnd).set_Size(new Size(64, 17));
			((Control)lblEnd).set_TabIndex(3);
			((Control)lblEnd).set_Text("12:10:10");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(12, 102));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(155, 17));
			((Control)label3).set_TabIndex(4);
			((Control)label3).set_Text("Approximate event time");
			((Control)txtHevent).set_Location(new Point(171, 96));
			((Control)txtHevent).set_Name("txtHevent");
			((Control)txtHevent).set_Size(new Size(21, 20));
			((Control)txtHevent).set_TabIndex(6);
			((Control)txtHevent).add_Enter((EventHandler)txtHevent_Enter);
			((Control)txtMevent).set_Location(new Point(198, 96));
			((Control)txtMevent).set_Name("txtMevent");
			((Control)txtMevent).set_Size(new Size(21, 20));
			((Control)txtMevent).set_TabIndex(7);
			((Control)txtMevent).add_Enter((EventHandler)txtMevent_Enter);
			((Control)txtSevent).set_Location(new Point(225, 96));
			((Control)txtSevent).set_Name("txtSevent");
			((Control)txtSevent).set_Size(new Size(21, 20));
			((Control)txtSevent).set_TabIndex(8);
			((Control)txtSevent).add_Enter((EventHandler)txtSevent_Enter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(175, 80));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(13, 13));
			((Control)label5).set_TabIndex(12);
			((Control)label5).set_Text("h");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(201, 80));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(15, 13));
			((Control)label6).set_TabIndex(13);
			((Control)label6).set_Text("m");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(229, 80));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(12, 13));
			((Control)label7).set_TabIndex(14);
			((Control)label7).set_Text("s");
			((Control)cmdOK).set_Location(new Point(112, 152));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(55, 27));
			((Control)cmdOK).set_TabIndex(15);
			((Control)cmdOK).set_Text("Return");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(170, 122));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(18, 20));
			((Control)label4).set_TabIndex(16);
			((Control)label4).set_Text("±");
			((Control)txtSemiDuration).set_Location(new Point(190, 123));
			((Control)txtSemiDuration).set_Name("txtSemiDuration");
			((Control)txtSemiDuration).set_Size(new Size(37, 20));
			((Control)txtSemiDuration).set_TabIndex(17);
			((Control)txtSemiDuration).set_Text("60");
			((Control)txtSemiDuration).add_Enter((EventHandler)txtSemiDuration_Enter);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(229, 127));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(29, 13));
			((Control)label8).set_TabIndex(18);
			((Control)label8).set_Text("secs");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(264, 191));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)txtSemiDuration);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)txtSevent);
			((Control)this).get_Controls().Add((Control)(object)txtMevent);
			((Control)this).get_Controls().Add((Control)(object)txtHevent);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)lblEnd);
			((Control)this).get_Controls().Add((Control)(object)lblStart);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Control)this).set_Name("Unistellarscope_SetTimes");
			((Control)this).set_Text("Unistellar .csv file: set time span for read ");
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
