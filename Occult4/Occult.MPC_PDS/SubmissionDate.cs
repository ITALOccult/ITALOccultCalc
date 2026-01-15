using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult.MPC_PDS
{
	public class SubmissionDate : Form
	{
		public int Year = 2024;

		public int Month = 1;

		public int Day = 1;

		private IContainer components;

		private Button cmdSubmissionOK;

		private Label label15;

		private Label label14;

		private Label label13;

		private Label label12;

		private ComboBox cmbDay;

		private ComboBox cmbMonth;

		private NumericUpDown updnYear;

		private Button cmdCancel;

		public SubmissionDate()
		{
			InitializeComponent();
			((ListControl)cmbMonth).set_SelectedIndex(0);
			((ListControl)cmbDay).set_SelectedIndex(0);
		}

		private void cmdSubmissionOK_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)1);
			Exit();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
		}

		private void Exit()
		{
			Year = (int)updnYear.get_Value();
			Month = ((ListControl)cmbMonth).get_SelectedIndex() + 1;
			Day = ((ListControl)cmbDay).get_SelectedIndex() + 1;
			((Form)this).Close();
		}

		private void SubmissionDate_FormClosing(object sender, FormClosingEventArgs e)
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
			//IL_06ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f4: Expected O, but got Unknown
			cmdSubmissionOK = new Button();
			label15 = new Label();
			label14 = new Label();
			label13 = new Label();
			label12 = new Label();
			cmbDay = new ComboBox();
			cmbMonth = new ComboBox();
			updnYear = new NumericUpDown();
			cmdCancel = new Button();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)this).SuspendLayout();
			((Control)cmdSubmissionOK).set_Location(new Point(189, 49));
			((Control)cmdSubmissionOK).set_Name("cmdSubmissionOK");
			((Control)cmdSubmissionOK).set_Size(new Size(52, 28));
			((Control)cmdSubmissionOK).set_TabIndex(7);
			((Control)cmdSubmissionOK).set_Text("OK");
			((ButtonBase)cmdSubmissionOK).set_UseVisualStyleBackColor(true);
			((Control)cmdSubmissionOK).add_Click((EventHandler)cmdSubmissionOK_Click);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(12, 9));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(304, 13));
			((Control)label15).set_TabIndex(6);
			((Control)label15).set_Text("Set submission date to clear MPC submission entries");
			label15.set_TextAlign(ContentAlignment.TopCenter);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(129, 40));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(26, 13));
			((Control)label14).set_TabIndex(5);
			((Control)label14).set_Text("Day");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(83, 40));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(37, 13));
			((Control)label13).set_TabIndex(4);
			((Control)label13).set_Text("Month");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(29, 40));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(29, 13));
			((Control)label12).set_TabIndex(3);
			((Control)label12).set_Text("Year");
			((ListControl)cmbDay).set_FormattingEnabled(true);
			cmbDay.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbDay).set_Location(new Point(136, 54));
			((Control)cmbDay).set_Name("cmbDay");
			((Control)cmbDay).set_Size(new Size(41, 21));
			((Control)cmbDay).set_TabIndex(2);
			((ListControl)cmbMonth).set_FormattingEnabled(true);
			cmbMonth.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbMonth).set_Location(new Point(84, 54));
			((Control)cmbMonth).set_Name("cmbMonth");
			((Control)cmbMonth).set_Size(new Size(40, 21));
			((Control)cmbMonth).set_TabIndex(1);
			((Control)updnYear).set_Location(new Point(26, 54));
			updnYear.set_Maximum(new decimal(new int[4] { 2040, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 2024, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(46, 20));
			((Control)updnYear).set_TabIndex(0);
			updnYear.set_Value(new decimal(new int[4] { 2024, 0, 0, 0 }));
			((Control)cmdCancel).set_Location(new Point(253, 49));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(52, 28));
			((Control)cmdCancel).set_TabIndex(8);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(327, 95));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)cmdSubmissionOK);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)cmbDay);
			((Control)this).get_Controls().Add((Control)(object)cmbMonth);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("SubmissionDate");
			((Control)this).set_Text("Set submission date");
			((Form)this).add_FormClosing(new FormClosingEventHandler(SubmissionDate_FormClosing));
			((ISupportInitialize)updnYear).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
