using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult
{
	public class UpdatesText : Form
	{
		private IContainer components;

		internal TextBox txtUpdate;

		public UpdatesText()
		{
			InitializeComponent();
		}

		private void UpdatesText_Resize(object sender, EventArgs e)
		{
			((Control)txtUpdate).set_Width(((Control)this).get_Width() - 24);
			((Control)txtUpdate).set_Height(((Control)this).get_Height() - 43);
		}

		private void UpdatesText_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(UpdatesText));
			txtUpdate = new TextBox();
			((Control)this).SuspendLayout();
			((Control)txtUpdate).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUpdate).set_Location(new Point(12, 12));
			((TextBoxBase)txtUpdate).set_Multiline(true);
			((Control)txtUpdate).set_Name("txtUpdate");
			txtUpdate.set_ScrollBars((ScrollBars)2);
			((Control)txtUpdate).set_Size(new Size(679, 331));
			((Control)txtUpdate).set_TabIndex(0);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(697, 348));
			((Control)this).get_Controls().Add((Control)(object)txtUpdate);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Control)this).set_MinimumSize(new Size(200, 100));
			((Control)this).set_Name("UpdatesText");
			((Form)this).set_StartPosition((FormStartPosition)4);
			((Control)this).set_Text("List of program changes made at each version");
			((Form)this).add_Load((EventHandler)UpdatesText_Load);
			((Control)this).add_Resize((EventHandler)UpdatesText_Resize);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
