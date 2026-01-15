using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Shapes
{
	public class AsteroidFromDamitModel : Form
	{
		private IContainer components;

		private Label label1;

		private TextBox txtNumber;

		private TextBox txtAsteroid;

		private Label label2;

		public AsteroidFromDamitModel()
		{
			InitializeComponent();
		}

		private void txtNumber_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()));
		}

		private void txtNumber_TextChanged(object sender, EventArgs e)
		{
			((Control)txtAsteroid).set_Text(GetShapeModelData.AsteroidNumberFromDamitID(((Control)txtNumber).get_Text()));
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
			//IL_0117: Unknown result type (might be due to invalid IL or missing references)
			//IL_0121: Expected O, but got Unknown
			label1 = new Label();
			txtNumber = new TextBox();
			txtAsteroid = new TextBox();
			label2 = new Label();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(29, 9));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(51, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Damit #");
			((Control)txtNumber).set_Location(new Point(28, 30));
			((Control)txtNumber).set_Name("txtNumber");
			((Control)txtNumber).set_Size(new Size(53, 20));
			((Control)txtNumber).set_TabIndex(1);
			((Control)txtNumber).add_TextChanged((EventHandler)txtNumber_TextChanged);
			((Control)txtNumber).add_KeyPress(new KeyPressEventHandler(txtNumber_KeyPress));
			((Control)txtAsteroid).set_Location(new Point(132, 30));
			((Control)txtAsteroid).set_Name("txtAsteroid");
			((TextBoxBase)txtAsteroid).set_ReadOnly(true);
			((Control)txtAsteroid).set_Size(new Size(60, 20));
			((Control)txtAsteroid).set_TabIndex(2);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(136, 9));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(53, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("Asteroid");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(Color.PaleTurquoise);
			((Form)this).set_ClientSize(new Size(220, 58));
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)txtAsteroid);
			((Control)this).get_Controls().Add((Control)(object)txtNumber);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("AsteroidFromDamitModel");
			((Control)this).set_Text("Asteroid from Damit Model");
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
