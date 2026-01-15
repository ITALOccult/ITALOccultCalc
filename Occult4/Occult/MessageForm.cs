using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MessageForm : Form
	{
		internal bool Cancel;

		private IContainer components;

		public Label label;

		private Panel panel1;

		private Panel panel2;

		internal Button cmdCancel;

		internal Button cmdExit;

		public MessageForm()
		{
			InitializeComponent();
			Cancel = false;
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdExit).set_Visible(true);
			Cancel = false;
			((Form)this).set_StartPosition((FormStartPosition)0);
			((Control)this).set_Top(10);
			((Control)this).set_Left(20);
		}

		internal void cmdCancel_Click(object sender, EventArgs e)
		{
			Cancel = true;
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
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
			label = new Label();
			panel1 = new Panel();
			panel2 = new Panel();
			cmdCancel = new Button();
			cmdExit = new Button();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label).set_AutoSize(true);
			((Control)label).set_BackColor(SystemColors.ButtonFace);
			((Control)label).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label).set_ForeColor(SystemColors.ActiveCaptionText);
			((Control)label).set_Location(new Point(8, 12));
			((Control)label).set_Name("label");
			((Control)label).set_Size(new Size(100, 17));
			((Control)label).set_TabIndex(0);
			((Control)label).set_Text("Downloading");
			((Control)panel1).set_BackColor(Color.RoyalBlue);
			((Control)panel1).get_Controls().Add((Control)(object)panel2);
			((Control)panel1).set_Location(new Point(2, 2));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(396, 46));
			((Control)panel1).set_TabIndex(1);
			((Control)panel2).set_BackColor(SystemColors.ButtonFace);
			((Control)panel2).get_Controls().Add((Control)(object)cmdExit);
			((Control)panel2).get_Controls().Add((Control)(object)cmdCancel);
			((Control)panel2).get_Controls().Add((Control)(object)label);
			((Control)panel2).set_Location(new Point(3, 3));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(390, 40));
			((Control)panel2).set_TabIndex(1);
			((Control)cmdCancel).set_BackColor(Color.Red);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_ForeColor(Color.Yellow);
			((Control)cmdCancel).set_Location(new Point(309, 7));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(55, 30));
			((Control)cmdCancel).set_TabIndex(1);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(false);
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ButtonBase)cmdExit).set_Image((Image)Resources.error);
			((Control)cmdExit).set_Location(new Point(368, 0));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(22, 22));
			((Control)cmdExit).set_TabIndex(3);
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(Color.Black);
			((Form)this).set_ClientSize(new Size(400, 50));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)0);
			((Control)this).set_Name("MessageForm");
			((Control)this).set_Text("Form1");
			((Control)panel1).ResumeLayout(false);
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)this).ResumeLayout(false);
		}
	}
}
