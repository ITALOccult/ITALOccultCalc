using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult
{
	public class PBar : Form
	{
		private IContainer components;

		internal ProgressBar pBarFTP;

		public PBar()
		{
			InitializeComponent();
		}

		public void Devents()
		{
			Application.DoEvents();
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
			pBarFTP = new ProgressBar();
			((Control)this).SuspendLayout();
			((Control)pBarFTP).set_Location(new Point(0, 2));
			((Control)pBarFTP).set_Name("pBarFTP");
			((Control)pBarFTP).set_Size(new Size(584, 14));
			((Control)pBarFTP).set_TabIndex(0);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 12f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(588, 20));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)pBarFTP);
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)5);
			((Control)this).set_Name("PBar");
			((Control)this).set_Text("PBar");
			((Control)this).ResumeLayout(false);
		}
	}
}
