using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult
{
	public class TimedMessageDisplay : Form
	{
		internal Timer timer = new Timer();

		private IContainer components;

		public TextBox txtMessage;

		public TimedMessageDisplay()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			InitializeComponent();
			Timer val = new Timer();
			val.set_Interval(3000);
			val.add_Tick((EventHandler)FormTimer_Tick);
			val.Start();
		}

		private void TimedMessageDisplay_Load(object sender, EventArgs e)
		{
			timer.set_Interval(5000);
		}

		private void FormTimer_Tick(object sender, EventArgs e)
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(TimedMessageDisplay));
			txtMessage = new TextBox();
			((Control)this).SuspendLayout();
			((Control)txtMessage).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMessage).set_Location(new Point(3, 0));
			((TextBoxBase)txtMessage).set_Multiline(true);
			((Control)txtMessage).set_Name("txtMessage");
			((Control)txtMessage).set_Size(new Size(359, 68));
			((Control)txtMessage).set_TabIndex(0);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(363, 69));
			((Control)this).get_Controls().Add((Control)(object)txtMessage);
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("TimedMessageDisplay");
			((Control)this).set_Text("Timed  Message  Display");
			((Form)this).add_Load((EventHandler)TimedMessageDisplay_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
