using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace VDTimer
{
	public class frmTimeInput : Form
	{
		private IContainer components;

		private DateTimePicker dtpStop;

		private DateTimePicker dtpStart;

		private Label lblStart;

		private Label lblStop;

		private Button btnOK;

		private Button btnCancel;

		private Label lblComment;

		private TextBox txtComment;

		private Label label1;

		public DateTime StartTime
		{
			get
			{
				return dtpStart.get_Value();
			}
			set
			{
				dtpStart.set_Value(value);
			}
		}

		public DateTime StopTime
		{
			get
			{
				return dtpStop.get_Value();
			}
			set
			{
				dtpStop.set_Value(value);
			}
		}

		public string Comment
		{
			get
			{
				return ((Control)txtComment).get_Text();
			}
			set
			{
				((Control)txtComment).set_Text(value);
			}
		}

		public frmTimeInput(string title)
		{
			InitializeComponent();
			((Control)this).set_Text(title);
			DateTime now = DateTime.Now;
			dtpStart.set_Value(now);
			dtpStop.set_Value(now);
		}

		private void btnOK_Click(object sender, EventArgs e)
		{
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			if (StopTime > StartTime)
			{
				((Form)this).set_DialogResult((DialogResult)1);
			}
			else
			{
				MessageBox.Show("Stop time must occur later than the start time.", "Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private void frmTimeInput_Load(object sender, EventArgs e)
		{
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
			dtpStop = new DateTimePicker();
			dtpStart = new DateTimePicker();
			lblStart = new Label();
			lblStop = new Label();
			btnOK = new Button();
			btnCancel = new Button();
			lblComment = new Label();
			txtComment = new TextBox();
			label1 = new Label();
			((Control)this).SuspendLayout();
			dtpStop.set_CustomFormat("yyyy-MM-dd  HH:mm:ss");
			dtpStop.set_Format((DateTimePickerFormat)8);
			((Control)dtpStop).set_Location(new Point(68, 40));
			((Control)dtpStop).set_Name("dtpStop");
			((Control)dtpStop).set_Size(new Size(152, 21));
			((Control)dtpStop).set_TabIndex(3);
			dtpStart.set_CustomFormat("yyyy-MM-dd  HH:mm:ss");
			dtpStart.set_Format((DateTimePickerFormat)8);
			((Control)dtpStart).set_Location(new Point(68, 12));
			((Control)dtpStart).set_Name("dtpStart");
			((Control)dtpStart).set_Size(new Size(152, 21));
			((Control)dtpStart).set_TabIndex(1);
			((Control)lblStart).set_AutoSize(true);
			((Control)lblStart).set_Location(new Point(8, 16));
			((Control)lblStart).set_Name("lblStart");
			((Control)lblStart).set_Size(new Size(49, 13));
			((Control)lblStart).set_TabIndex(0);
			((Control)lblStart).set_Text("Start At:");
			((Control)lblStop).set_AutoSize(true);
			((Control)lblStop).set_Location(new Point(8, 44));
			((Control)lblStop).set_Name("lblStop");
			((Control)lblStop).set_Size(new Size(47, 13));
			((Control)lblStop).set_TabIndex(2);
			((Control)lblStop).set_Text("Stop At:");
			((Control)btnOK).set_Location(new Point(47, 97));
			((Control)btnOK).set_Name("btnOK");
			((Control)btnOK).set_Size(new Size(48, 23));
			((Control)btnOK).set_TabIndex(6);
			((Control)btnOK).set_Text("OK");
			((ButtonBase)btnOK).set_UseVisualStyleBackColor(true);
			((Control)btnOK).add_Click((EventHandler)btnOK_Click);
			btnCancel.set_DialogResult((DialogResult)2);
			((Control)btnCancel).set_Location(new Point(119, 97));
			((Control)btnCancel).set_Name("btnCancel");
			((Control)btnCancel).set_Size(new Size(64, 23));
			((Control)btnCancel).set_TabIndex(7);
			((Control)btnCancel).set_Text("Cancel");
			((ButtonBase)btnCancel).set_UseVisualStyleBackColor(true);
			((Control)lblComment).set_AutoSize(true);
			((Control)lblComment).set_Location(new Point(8, 72));
			((Control)lblComment).set_Name("lblComment");
			((Control)lblComment).set_Size(new Size(56, 13));
			((Control)lblComment).set_TabIndex(4);
			((Control)lblComment).set_Text("Comment:");
			((Control)txtComment).set_Location(new Point(68, 68));
			((Control)txtComment).set_Name("txtComment");
			((Control)txtComment).set_Size(new Size(152, 21));
			((Control)txtComment).set_TabIndex(5);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_BackColor(Color.Ivory);
			((Control)label1).set_Font(new Font("Tahoma", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.Maroon);
			((Control)label1).set_Location(new Point(1, 124));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(229, 12));
			((Control)label1).set_TabIndex(8);
			((Control)label1).set_Text("Make sure there are at least 8 secs between events");
			((Form)this).set_AcceptButton((IButtonControl)(object)btnOK);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_CancelButton((IButtonControl)(object)btnCancel);
			((Form)this).set_ClientSize(new Size(231, 138));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtComment);
			((Control)this).get_Controls().Add((Control)(object)lblComment);
			((Control)this).get_Controls().Add((Control)(object)btnCancel);
			((Control)this).get_Controls().Add((Control)(object)btnOK);
			((Control)this).get_Controls().Add((Control)(object)lblStop);
			((Control)this).get_Controls().Add((Control)(object)lblStart);
			((Control)this).get_Controls().Add((Control)(object)dtpStop);
			((Control)this).get_Controls().Add((Control)(object)dtpStart);
			((Control)this).set_Font(new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Form)this).set_Location(new Point(50, 50));
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("frmTimeInput");
			((Form)this).set_StartPosition((FormStartPosition)0);
			((Control)this).set_Text("Time Input");
			((Form)this).set_TopMost(true);
			((Form)this).add_Load((EventHandler)frmTimeInput_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
