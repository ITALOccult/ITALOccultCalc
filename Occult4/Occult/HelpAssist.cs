using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class HelpAssist : Form
	{
		private IContainer components;

		private Panel panel1;

		private Label label5;

		private Label label6;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private PictureBox pictureBox1;

		private Label label9;

		private Label label7;

		private Label label8;

		private Label label10;

		public HelpAssist()
		{
			InitializeComponent();
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
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(HelpAssist));
			panel1 = new Panel();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			pictureBox1 = new PictureBox();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)pictureBox1).BeginInit();
			((Control)this).SuspendLayout();
			((ScrollableControl)panel1).set_AutoScroll(true);
			((ScrollableControl)panel1).set_AutoScrollMargin(new Size(0, 10));
			((ScrollableControl)panel1).set_AutoScrollMinSize(new Size(470, 700));
			panel1.set_AutoSizeMode((AutoSizeMode)0);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)label8);
			((Control)panel1).get_Controls().Add((Control)(object)pictureBox1);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)label6);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).set_Location(new Point(6, 11));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(494, 395));
			((Control)panel1).set_TabIndex(0);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(10, 12));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(447, 34));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("This form provides advice if you are unable to view the help \r\ntopics when you select Help on any of the forms in Occult\r\n");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(10, 59));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(242, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("There are several possible causes to this problem:");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(10, 89));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(225, 13));
			((Control)label3).set_TabIndex(2);
			((Control)label3).set_Text("1. Blocked by Windows security policy");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(10, 110));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(380, 104));
			((Control)label4).set_TabIndex(3);
			((Control)label4).set_Text(componentResourceManager.GetString("label4.Text"));
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(10, 843));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(402, 39));
			((Control)label5).set_TabIndex(5);
			((Control)label5).set_Text("Type in the following to your command prompt: \"regsvr32 hhctrl.ocx\". \r\nAfter getting the success message like \"DllRegisterServer in hhctrl.ocx succeeded\",\r\ntry to open your CHM file again.\r\n");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(10, 822));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(455, 13));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("2. The HTML Help (CHM) viewer component is not registered on your computer");
			pictureBox1.set_Image((Image)Occult.Properties.Resources.CHMfix);
			((Control)pictureBox1).set_Location(new Point(26, 273));
			((Control)pictureBox1).set_Name("pictureBox1");
			((Control)pictureBox1).set_Size(new Size(368, 513));
			pictureBox1.set_SizeMode((PictureBoxSizeMode)2);
			pictureBox1.set_TabIndex(6);
			pictureBox1.set_TabStop(false);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(11, 927));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(406, 65));
			((Control)label7).set_TabIndex(8);
			((Control)label7).set_Text(componentResourceManager.GetString("label7.Text"));
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(11, 906));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(161, 13));
			((Control)label8).set_TabIndex(7);
			((Control)label8).set_Text("3. Other causes / solutions");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_BackColor(Color.DarkBlue);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.Yellow);
			((Control)label9).set_Location(new Point(10, 254));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(377, 13));
			((Control)label9).set_TabIndex(9);
			((Control)label9).set_Text("The 'Unblock' check box is not present if Windows is not blocking the Help file");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(10, 224));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(298, 26));
			((Control)label10).set_TabIndex(10);
			((Control)label10).set_Text("The following graphic illustrates the location of the check box \r\n( the displayed details of the chm file do not relate to Occult )");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(512, 411));
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("HelpAssist");
			((Control)this).set_Text("Help Assistance");
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)pictureBox1).EndInit();
			((Control)this).ResumeLayout(false);
		}
	}
}
